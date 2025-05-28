package turtleMart.order.dto.response;

import turtleMart.order.entity.OrderItem;

public record RefundResponse(
        Long memberId,
        Long orderId,
        Long orderItemId,
        String productName,
        String optionInfo, //옵션 상세 정보
        Integer quantity,
        Integer price,
        Integer totalPrice
) {
    public static RefundResponse from(OrderItem orderItem, String optionInfo) {
        return new RefundResponse(
                orderItem.getOrder().getMember().getId(),
                orderItem.getOrder().getId(),
                orderItem.getId(),
                orderItem.getName(),
                optionInfo,
                orderItem.getQuantity(),
                orderItem.getPrice(),
                orderItem.getQuantity() * orderItem.getPrice()
        );
    }
}
