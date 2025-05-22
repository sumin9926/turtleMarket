package turtleMart.order.dto.response;

import turtleMart.order.entity.OrderItem;

public record OrderItemResponse(
        Long productId,
        Integer productPrice,
        String productName,
        Integer quantity
) {
    public static OrderItemResponse from(OrderItem orderItem) {
        return new OrderItemResponse(
                orderItem.getProduct().getId(),
                orderItem.getPrice(),
                orderItem.getName(),
                orderItem.getQuantity()
        );
    }
}
