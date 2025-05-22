package turtleMart.order.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import turtleMart.member.repository.MemberRepository;
import turtleMart.order.dto.request.AddCartItemRequest;
import turtleMart.order.dto.request.CartOrderSheetRequest;
import turtleMart.order.dto.request.OrderItemStatusRequest;
import turtleMart.order.dto.response.OrderDetailResponse;
import turtleMart.order.dto.response.OrderItemResponse;
import turtleMart.order.dto.response.OrderSheetResponse;
import turtleMart.order.entity.Order;
import turtleMart.order.entity.OrderItem;
import turtleMart.order.entity.OrderItemStatus;
import turtleMart.order.repository.OrderItemRepository;
import turtleMart.order.repository.OrderRepository;
import turtleMart.product.entity.Product;
import turtleMart.product.repository.ProductRepository;

import java.util.ArrayList;
import java.util.List;

@Service
@RequiredArgsConstructor
public class OrderService {

    private final ProductRepository productRepository;
    private final OrderRepository orderRepository;
    private final MemberRepository memberRepository;
    private final CartService cartService;
    private final OrderItemRepository orderItemRepository;

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

            if(!order.getOrderItems().contains(orderItem)){
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
}
