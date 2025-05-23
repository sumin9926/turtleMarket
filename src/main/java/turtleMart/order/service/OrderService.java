package turtleMart.order.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import turtleMart.delivery.entity.Delivery;
import turtleMart.delivery.repository.DeliveryRepository;
import turtleMart.member.entity.Member;
import turtleMart.member.repository.MemberRepository;
import turtleMart.member.repository.SellerRepository;
import turtleMart.order.dto.request.AddCartItemRequest;
import turtleMart.order.dto.request.CartOrderSheetRequest;
import turtleMart.order.dto.request.OrderItemStatusRequest;
import turtleMart.order.dto.request.OrderRequest;
import turtleMart.order.dto.response.*;
import turtleMart.order.entity.Order;
import turtleMart.order.entity.OrderItem;
import turtleMart.order.entity.OrderItemStatus;
import turtleMart.order.repository.OrderItemRepository;
import turtleMart.order.repository.OrderRepository;
import turtleMart.payment.dto.request.PaymentRequest;
import turtleMart.product.entity.Product;
import turtleMart.product.repository.ProductRepository;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class OrderService {

    private final ProductRepository productRepository;
    private final OrderRepository orderRepository;
    private final MemberRepository memberRepository;
    private final OrderItemRepository orderItemRepository;
    private final DeliveryRepository deliveryRepository;
    private final SellerRepository sellerRepository;
    private final CartService cartService;
    private final RedisTemplate<String, String> redisTemplate;
    private final KafkaTemplate<String, Object> kafkaTemplate;

    @Value("${kafka.topic.payment}")
    private String paymentTopic;

    @Transactional(readOnly = true)
    public List<OrderSheetResponse> getCartOrderSheet(List<CartOrderSheetRequest> orderSheetList, Long memberId) {

        if (!memberRepository.existsById(memberId)) {
            throw new RuntimeException("존재하지 않는 회원입니다.");//TODO 커스텀 예외처리
        }

        /*TODO 가격/품절상태 검증 필요*/

        List<OrderSheetResponse> responseList = new ArrayList<>();

        for (CartOrderSheetRequest orderSheet : orderSheetList) {
            Product product = productRepository.findById(orderSheet.productId()).orElseThrow(
                    () -> new RuntimeException("상품이 존재하지 않습니다.") //TODO 커스텀 예외처리
            );
            OrderSheetResponse response = new OrderSheetResponse(
                    product.getId(), product.getName(), product.getPrice(), orderSheet.quantity()
            );
            responseList.add(response);
        }

        return responseList;
    }

    public OrderSheetResponse getDirectOrderSheet(CartOrderSheetRequest request, Long memberId) {

        if (!memberRepository.existsById(memberId)) {
            throw new RuntimeException("존재하지 않는 회원입니다.");//TODO 커스텀 예외처리
        }

        /*TODO 가격/품절상태 검증 필요*/

        // 주문서에 들어갈 정보 보내기
        Product product = productRepository.findById(request.productId()).orElseThrow(
                () -> new RuntimeException("상품이 존재하지 않습니다.") //TODO 커스텀 예외처리
        );
        OrderSheetResponse response = new OrderSheetResponse(
                product.getId(), product.getName(), product.getPrice(), request.quantity()
        );

        // 장바구니에 해당 상품 담기
        try {
            cartService.addItemsToCart(memberId, new AddCartItemRequest(request.productId(), request.quantity()));
        } catch (JsonProcessingException e) {
            throw new RuntimeException("Redis 장바구니 상품 추가 실패", e);
        }

        return response;
    }

    @Transactional(readOnly = true)
    public OrderDetailResponse getOrderDetail(Long memberId, Long orderId) {

        if (!memberRepository.existsById(memberId)) {
            throw new RuntimeException("존재하지 않는 회원입니다.");//TODO 커스텀 예외처리
        }

        Order order = orderRepository.findWithOrderItemsById(orderId).orElseThrow(
                () -> new RuntimeException("존재하지 않는 주문 입니다.")//TODO 커스텀 예외처리
        );

        List<OrderItemResponse> orderItemResponseList = order.getOrderItems().stream()
                .map(OrderItemResponse::from)
                .toList();

        return OrderDetailResponse.from(order, orderItemResponseList);
    }

    @Transactional
    public OrderDetailResponse updateOrderItemStatus(
            Long orderId, List<Long> orderItemIdList, OrderItemStatusRequest request, Long memberId
    ) {
        if (!memberRepository.existsById(memberId)) {
            throw new RuntimeException("존재하지 않는 회원입니다.");//TODO 커스텀 예외처리
        }

        Order order = orderRepository.findById(orderId).orElseThrow(
                () -> new RuntimeException("존재하지 않는 주문서 입니다.")//TODO 커스텀 예외처리
        );

        OrderItemStatus newStatus = OrderItemStatusRequest.checkOrderItemStatusIgnoreCase(request.orderItemStatus());
        List<OrderItem> updatedOrderItemList = new ArrayList<>();
        // orderItemList 를 for 문으로 하나씩 조회해서 업데이트 내역을 반영하고 List 형태로 저장
        for (Long orderItemId : orderItemIdList) {
            OrderItem orderItem = orderItemRepository.findById(orderItemId).orElseThrow(
                    () -> new RuntimeException("존재하지 않는 상품 주문 내역입니다.")//TODO 커스텀 예외처리
            );

            if (!order.getOrderItems().contains(orderItem)) {
                throw new RuntimeException("해당 주문에 속하지 않은 상품입니다."); //TODO 커스텀 예외처리
            }

            // 갱신 순서를 지켜야한다.
            OrderItemStatus currentStatus = orderItem.getOrderItemStatus();
            if (!canTransition(currentStatus, newStatus)) {
                throw new IllegalStateException(
                        String.format("상태 변경 불가: 현재 상태 '%s'에서 '%s'(으)로 변경할 수 없습니다.",
                                currentStatus, newStatus)
                );
            } else {
                orderItem.updateStatus(newStatus);
                updatedOrderItemList.add(orderItem);
            }
        }

        List<OrderItemResponse> orderItemResponseList = updatedOrderItemList.stream().map(OrderItemResponse::from).toList();

        return OrderDetailResponse.from(order, orderItemResponseList);
    }

    public boolean canTransition(OrderItemStatus current, OrderItemStatus target) {
        return switch (current) {
            case UNPAID -> target == OrderItemStatus.CANCELED || target == OrderItemStatus.ORDERED;
            case CANCELED -> target == OrderItemStatus.ORDERED;
            case ORDERED -> target == OrderItemStatus.REFUNDING || target == OrderItemStatus.CONFIRMED;
            case REFUNDING -> target == OrderItemStatus.REFUNDED;
            case REFUNDED, CONFIRMED -> false;
        };
    }

    @Transactional(readOnly = true)
    public MemberOrderListResponse getOrderList(Long memberId) {
        if (!memberRepository.existsById(memberId)) {
            throw new RuntimeException("존재하지 않는 회원입니다.");//TODO 커스텀 예외처리
        }

        List<OrderSimpleResponse> simpleResponseList = new ArrayList<>();

        List<Order> orderList = orderRepository.findWithOrderItemsByMemberId(memberId);
        List<Long> orderIdList = orderList.stream().map(Order::getId).toList();
        List<Delivery> deliveryList = deliveryRepository.findAllWithOrderIds(orderIdList);
        Map<Long, Delivery> deliveryMap = deliveryList.stream().collect(Collectors.toMap(d -> d.getOrder().getId(), d -> d));

        for (Order order : orderList) {
            // orderId로 deliveryStatus 가져오기
            Delivery delivery = deliveryMap.get(order.getId());
            if (null==delivery) {
                throw new RuntimeException("배송 내역이 존재하지 않습니다.");
            }
            List<Long> orderItemIdList = order.getOrderItems().stream().map(OrderItem::getId).toList();
            simpleResponseList.add(OrderSimpleResponse.from(order, delivery, orderItemIdList));
        }

        return MemberOrderListResponse.from(memberId, simpleResponseList);
    }

    public TotalOrderedQuantityResponse getTotalOrderedQuantity(
            Long sellerId, Long productId, LocalDate startDate, LocalDate endDate
    ) {
        if(!sellerRepository.existsById(sellerId)){
            throw new RuntimeException("존재하지 않는 판매자입니다.");//TODO 커스텀 예외처리
        }

        Integer totalOrderedQuantity = orderItemRepository.countTotalOrderedBySellerAndProduct(
                sellerId, productId, startDate.atStartOfDay(), endDate.plusDays(1).atStartOfDay()
        );

        return TotalOrderedQuantityResponse.from(productId, totalOrderedQuantity);
    }

    @Transactional
    public void createOrder(Long memberId, List<CartOrderSheetRequest> itemList, OrderRequest request) {
        Member member = memberRepository.findById(memberId).orElseThrow(
                () -> new RuntimeException("존재하지 않는 회원입니다.") //TODO 커스텀 예외처리
        );

        Order order = Order.of(member, new ArrayList<>(), 0);

        itemList.forEach(item -> {
            Product product = productRepository.findById(item.productId()).orElseThrow(
                    () -> new RuntimeException("존재하지 않는 상품입니다.")//TODO 커스텀 예외처리
            );
            OrderItem orderItem = OrderItem.of(order, product, product.getPrice(), product.getName(), item.quantity());
            order.addOrderItem(orderItem);
        });

        order.calculateTotalPrice();
        orderRepository.save(order);

        // 장바구니 Redis 캐시 삭제
        removeCartItemFromRedis(memberId, request.cartItemIdList());

        // Kafka로 결제 요청
        PaymentRequest paymentRequest =  PaymentRequest.from(order, memberId, request);

        kafkaTemplate.send(paymentTopic, paymentRequest);
    }

    private void removeCartItemFromRedis(Long memberId, List<Long> cartItemIdList) {
        for(Long cartItemId : cartItemIdList){
            String key = "cart:" + memberId;

            Boolean cartItemExist = redisTemplate.opsForHash().hasKey(key, String.valueOf(cartItemId));
            if (Boolean.FALSE.equals(cartItemExist)) {
                throw new RuntimeException("장바구니에서 삭제하려는 상품이 존재하지 않음"); //TODO 커스텀 예외처리
            }

            redisTemplate.opsForHash().delete(key, String.valueOf(cartItemId));
        }
    }
}
