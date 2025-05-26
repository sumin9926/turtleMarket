package turtleMart.order.dto.response;

import turtleMart.order.entity.Order;

import java.time.LocalDateTime;
import java.util.List;

public record OrderDetailResponse(
        Long orderId,
        Long memberId,
        List<OrderItemResponse> orderItemList,
        Integer totalPrice,
        LocalDateTime orderedAt
) {
    public static OrderDetailResponse from(Order order, List<OrderItemResponse> orderItemList) {
        return new OrderDetailResponse(
                order.getId(),
                order.getMember().getId(),
                orderItemList,
                order.getTotalPrice(),
                order.getOrderedAt()
        );
    }
}
