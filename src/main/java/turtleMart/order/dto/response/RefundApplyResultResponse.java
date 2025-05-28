package turtleMart.order.dto.response;

import turtleMart.order.entity.OrderItem;
import turtleMart.order.entity.OrderItemStatus;

import java.time.LocalDateTime;

public record RefundApplyResultResponse(
        Long memberId,
        Long orderItemId,
        OrderItemStatus orderItemStatus,
        LocalDateTime requestDate
) {
    public static RefundApplyResultResponse from(OrderItem orderItem){
        return new RefundApplyResultResponse(
                orderItem.getOrder().getMember().getId(),
                orderItem.getId(),
                orderItem.getOrderItemStatus(),
                LocalDateTime.now()
        );
    }
}
