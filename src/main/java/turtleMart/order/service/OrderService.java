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
import turtleMart.global.common.OptionDisplayUtils;
import turtleMart.global.exception.BadRequestException;
import turtleMart.global.exception.ConflictRequestException;
import turtleMart.global.exception.ErrorCode;
import turtleMart.global.exception.NotFoundException;
import turtleMart.global.kafka.dto.OperationWrapperDto;
import turtleMart.global.kafka.enums.OperationType;
import turtleMart.global.utill.JsonHelper;
import turtleMart.member.entity.Member;
import turtleMart.member.repository.MemberRepository;
import turtleMart.member.repository.SellerRepository;
import turtleMart.order.dto.request.*;
import turtleMart.order.dto.response.*;
import turtleMart.order.entity.Order;
import turtleMart.order.entity.OrderItem;
import turtleMart.order.entity.OrderItemStatus;
import turtleMart.order.repository.OrderItemRepository;
import turtleMart.order.repository.OrderRepository;
import turtleMart.payment.dto.request.PaymentRequest;
import turtleMart.product.entity.Product;
import turtleMart.product.entity.ProductOptionCombination;
import turtleMart.product.repository.ProductOptionCombinationRepository;
import turtleMart.product.repository.ProductOptionValueRepository;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class OrderService {

    private final OrderRepository orderRepository;
    private final MemberRepository memberRepository;
    private final OrderItemRepository orderItemRepository;
    private final DeliveryRepository deliveryRepository;
    private final SellerRepository sellerRepository;
    private final ProductOptionCombinationRepository productOptionCombinationRepository;
    private final ProductOptionValueRepository productOptionValueRepository;
    private final CartService cartService;
    private final RedisTemplate<String, String> redisTemplate;
    private final KafkaTemplate<String, String> stringKafkaTemplate;
    private final KafkaTemplate<String, Object> objectKafkaTemplate;

    @Value("${kafka.topic.payment}")
    private String paymentTopic;

    @Transactional(readOnly = true)
    public List<OrderSheetResponse> getOrderSheet(List<CartOrderSheetRequest> orderSheetList, Long memberId) {

        if (!memberRepository.existsById(memberId)) {
            throw new NotFoundException(ErrorCode.MEMBER_NOT_FOUND);
        }

        List<OrderSheetResponse> responseList = new ArrayList<>();

        for (CartOrderSheetRequest orderSheet : orderSheetList) {
            ProductOptionCombination option = productOptionCombinationRepository.findById(orderSheet.productOptionId()).orElseThrow(
                    () -> new NotFoundException(ErrorCode.PRODUCT_OPTION_COMBINATION_NOT_FOUND)
            );

            Product product = option.getProduct();

            if (null == product) {
                throw new NotFoundException(ErrorCode.PRODUCT_NOT_FOUND);
            }

            String optionInfo = OptionDisplayUtils.buildOptionDisplay(option.getUniqueKey(), productOptionValueRepository);

            OrderSheetResponse response = new OrderSheetResponse(orderSheet.productOptionId(), product.getId(),
                    optionInfo, product.getName(), product.getPrice(), orderSheet.quantity());

            responseList.add(response);
        }

        return responseList;
    }

    @Transactional
    public OrderSheetResponse getDirectOrderSheet(List<CartOrderSheetRequest> requests, Long memberId) {

        List<OrderSheetResponse> responseList = getOrderSheet(requests, memberId);

        if (responseList.isEmpty()) {
            throw new NotFoundException(ErrorCode.ORDER_SHEET_NOT_FOUND);
        }

        OrderSheetResponse response = responseList.get(0);

        // 장바구니에 해당 상품 담기
        try {
            cartService.addItemsToCart(memberId, new AddCartItemRequest(requests.get(0).productOptionId(), requests.get(0).quantity()));
        } catch (JsonProcessingException e) {
            throw new RuntimeException("Redis 장바구니 상품 추가 실패", e);
        }

        return response;
    }

    @Transactional(readOnly = true)
    public OrderDetailResponse getOrderDetail(Long memberId, Long orderId) {

        if (!memberRepository.existsById(memberId)) {
            throw new NotFoundException(ErrorCode.MEMBER_NOT_FOUND);
        }

        Order order = orderRepository.findWithOrderItemsById(orderId).orElseThrow(
                () -> new NotFoundException(ErrorCode.ORDER_NOT_FOUND)
        );

        List<OrderItemResponse> orderItemResponseList = order.getOrderItems().stream()
                .map(orderItem -> {
                    String optionInfo = OptionDisplayUtils.buildOptionDisplay(
                            orderItem.getProductOptionCombination().getUniqueKey(),
                            productOptionValueRepository
                    );
                    return OrderItemResponse.from(orderItem, optionInfo);
                })
                .toList();

        return OrderDetailResponse.from(order, orderItemResponseList);
    }

    @Transactional
    public OrderDetailResponse updateOrderItemStatus(
            Long orderId, List<Long> orderItemIdList, OrderItemStatusRequest request, Long memberId
    ) {
        if (!memberRepository.existsById(memberId)) {
            throw new NotFoundException(ErrorCode.MEMBER_NOT_FOUND);
        }

        Order order = orderRepository.findById(orderId).orElseThrow(
                () -> new NotFoundException(ErrorCode.ORDER_NOT_FOUND)
        );

        OrderItemStatus newStatus = OrderItemStatusRequest.checkOrderItemStatusIgnoreCase(request.orderItemStatus());
        List<OrderItem> updatedOrderItemList = new ArrayList<>();
        // orderItemList 를 for 문으로 하나씩 조회해서 업데이트 내역을 반영하고 List 형태로 저장
        for (Long orderItemId : orderItemIdList) {
            OrderItem orderItem = orderItemRepository.findById(orderItemId).orElseThrow(
                    () -> new NotFoundException(ErrorCode.ORDER_ITEM_NOT_FOUND)
            );

            if (!order.getOrderItems().contains(orderItem)) {
                throw new BadRequestException(ErrorCode.ORDER_ITEM_NOT_IN_ORDER);
            }

            // 갱신 순서를 지켜야한다.
            OrderItemStatus currentStatus = orderItem.getOrderItemStatus();
            currentStatus.validateTransitionTo(newStatus);
            orderItem.updateStatus(newStatus);
            updatedOrderItemList.add(orderItem);
        }

        List<OrderItemResponse> orderItemResponseList = updatedOrderItemList.stream()
                .map(orderItem -> {
                    String optionInfo = OptionDisplayUtils.buildOptionDisplay(
                            orderItem.getProductOptionCombination().getUniqueKey(),
                            productOptionValueRepository
                    );
                    return OrderItemResponse.from(orderItem, optionInfo);
                })
                .toList();

        return OrderDetailResponse.from(order, orderItemResponseList);
    }

    @Transactional(readOnly = true)
    public MemberOrderListResponse getOrderList(Long memberId) {
        if (!memberRepository.existsById(memberId)) {
            throw new NotFoundException(ErrorCode.MEMBER_NOT_FOUND);
        }

        List<OrderSimpleResponse> simpleResponseList = new ArrayList<>();

        List<Order> orderList = orderRepository.findWithOrderItemsByMemberId(memberId);
        List<Long> orderIdList = orderList.stream().map(Order::getId).toList();
        List<Delivery> deliveryList = deliveryRepository.findAllWithOrderIds(orderIdList);
        Map<Long, Delivery> deliveryMap = deliveryList.stream().collect(Collectors.toMap(d -> d.getOrder().getId(), d -> d));

        for (Order order : orderList) {
            // orderId로 deliveryStatus 가져오기
            Delivery delivery = deliveryMap.get(order.getId());
            if (null == delivery) {
                throw new NotFoundException(ErrorCode.DELIVERY_NOT_FOUND);
            }

            List<OrderItemResponse> orderItemResponses = order.getOrderItems().stream()
                    .map(orderItem -> {
                        String optionInfo = OptionDisplayUtils.buildOptionDisplay(
                                orderItem.getProductOptionCombination().getUniqueKey(),
                                productOptionValueRepository
                        );
                        return OrderItemResponse.from(orderItem, optionInfo);
                    })
                    .toList();

            simpleResponseList.add(OrderSimpleResponse.from(order, delivery, orderItemResponses));
        }

        return MemberOrderListResponse.from(memberId, simpleResponseList);
    }

    public TotalOrderedQuantityResponse getTotalOrderedQuantity(
            Long sellerId, Long productId, LocalDate startDate, LocalDate endDate
    ) {
        if (!sellerRepository.existsById(sellerId)) {
            throw new NotFoundException(ErrorCode.SELLER_NOT_FOUND);
        }

        Long totalOrderedQuantity = orderItemRepository.countTotalOrderedBySellerAndProduct(
                sellerId, productId, startDate.atStartOfDay(), endDate.plusDays(1).atStartOfDay(), OrderItemStatus.REFUNDED
        );

        return TotalOrderedQuantityResponse.from(productId, totalOrderedQuantity);
    }

    @Transactional
    public void tryOrder(Long memberId, List<CartOrderSheetRequest> itemList, OrderWrapperRequest wrapperRequest) {
        if (!memberRepository.existsById(memberId)) {
            throw new NotFoundException(ErrorCode.MEMBER_NOT_FOUND);
        }
        // 일단 DTO 먼저 만든다. (OrderWrapperRequest - OrderRequest+결재요청DTO+배송요청Dto)
        OrderWrapperRequest newWrapperRequest = OrderWrapperRequest.updateItemList(wrapperRequest, itemList);
        // 주문 요청이 들어오면 kafka 'order~' 토픽으로 일단 넘긴다.(그러면 그쪽에서 가격 상태 변경 상태인지 아닌지 검증한다.)
        List<Long> productOptionCombinationIdList = itemList.stream()
                .map(CartOrderSheetRequest::productOptionId)
                .toList();

        String key = JsonHelper.toJson(productOptionCombinationIdList);
        String payload = JsonHelper.toJson(newWrapperRequest);
        String value = JsonHelper.toJson(OperationWrapperDto.from(OperationType.ORDER_CREATE, payload));

        stringKafkaTemplate.send("order_make_topic", key, value);
    }

    @Transactional
    public void createOrder(OrderWrapperRequest wrapperRequest){
        // 카프카 통과해서 오면 실행되는 로직
        List<OrderRequest> orderRequestList = wrapperRequest.orderList();
        List<CartOrderSheetRequest> orderSheetRequestList = wrapperRequest.itemList();
        PaymentRequest paymentRequest = wrapperRequest.payment();

        Map<Long, CartOrderSheetRequest> orderSheetMap = orderSheetRequestList.stream()
                .collect(Collectors.toMap(
                        CartOrderSheetRequest::productOptionId,
                        Function.identity()
                ));
        Map<Long, OrderRequest> orderMap = orderRequestList.stream()
                .collect(Collectors.toMap(
                        OrderRequest::productOptionId,
                        Function.identity()
                ));

        // 가격 정합성 검증
        for(Long productOptionId : orderSheetMap.keySet()){
            Integer quantity = orderSheetMap.get(productOptionId).quantity();
            OrderRequest order = orderMap.get(productOptionId);
            ProductOptionCombination optionCombination = productOptionCombinationRepository.findById(productOptionId).orElseThrow(
                    () -> new NotFoundException(ErrorCode.PRODUCT_OPTION_COMBINATION_NOT_FOUND)
            );
            Integer currentPrice = optionCombination.getPrice();
            Integer snapShotPrice = order.price();

            if(!snapShotPrice.equals(currentPrice) || !order.totalPrice().equals(currentPrice*quantity)){
                throw new ConflictRequestException(ErrorCode.ORDER_PRICE_VALIDATION_FAILED);
            }
        }

        // 정합성 문제 없을 경우 결재 계속 진행
        Member member = memberRepository.findById(paymentRequest.memberId()).orElseThrow(
                () -> new NotFoundException(ErrorCode.MEMBER_NOT_FOUND)
        );
        Order order = Order.of(member, new ArrayList<>(), 0);

        orderSheetMap.keySet().forEach(productOptionId -> {
            ProductOptionCombination productOptionCombination = productOptionCombinationRepository.findById(productOptionId).orElseThrow(
                    () -> new NotFoundException(ErrorCode.PRODUCT_OPTION_COMBINATION_NOT_FOUND)
            );
            OrderItem orderItem = OrderItem.of(order, productOptionCombination, productOptionCombination.getPrice(),
                    productOptionCombination.getProduct().getName(), orderSheetMap.get(productOptionId).quantity());
            order.addOrderItem(orderItem);
        });

        order.calculateTotalPrice();
        orderRepository.save(order);

        List<Long> cartItemIdList = orderRequestList.stream()
                .map(OrderRequest::cartItemId)
                .toList();

        // 장바구니 Redis 캐시 삭제
        removeCartItemFromRedis(member.getId(), cartItemIdList);

        // Kafka 로 결제 요청(배송정보도 함께 담아서 전송)
        PaymentWrapperRequest paymentWrapperRequest = PaymentWrapperRequest.from(wrapperRequest.payment(), wrapperRequest.delivery());
        objectKafkaTemplate.send(paymentTopic, paymentWrapperRequest);
    }

    private void removeCartItemFromRedis(Long memberId, List<Long> cartItemIdList) {
        String key = "cart:" + memberId;

        for (Long cartItemId : cartItemIdList) {
            Boolean cartItemExist = redisTemplate.opsForHash().hasKey(key, String.valueOf(cartItemId));
            if (Boolean.FALSE.equals(cartItemExist)) {
                throw new NotFoundException(ErrorCode.PRODUCT_NOT_IN_CART);
            }

            redisTemplate.opsForHash().delete(key, String.valueOf(cartItemId));
        }
    }
}
