package turtleMart.order.dto.response;

import turtleMart.product.entity.Product;

public record OrderSheetResponse(
        Long productOptionId, //==productOptionCombinationId
        Long productId,
        String optionInfo,
        String productName,
        Integer productPrice,
        Integer quantity
) {
    public static OrderSheetResponse from(Long productOptionId, Product product, String optionInfo, Integer quantity) {
        return new OrderSheetResponse(
                productOptionId,
                product.getId(),
                optionInfo,
                product.getName(),
                product.getPrice(),
                quantity
        );
    }
}
