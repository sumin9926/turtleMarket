package turtleMart.order.dto.response;

import turtleMart.order.entity.OrderItem;

public record OrderItemResponse(
        Long productId,
        Long productOptionId,
        String optionInfo,
        Integer productPrice,
        String productName,
        Integer quantity
) {
    public static OrderItemResponse from(OrderItem orderItem, String optionInfo) {
        return new OrderItemResponse(
                orderItem.getProductOptionCombination().getProduct().getId(),
                orderItem.getProductOptionCombination().getId(),
                optionInfo,
                orderItem.getPrice(),
                orderItem.getName(),
                orderItem.getQuantity()
        );
    }
}
