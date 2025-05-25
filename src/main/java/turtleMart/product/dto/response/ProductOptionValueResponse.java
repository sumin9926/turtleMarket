package turtleMart.product.dto.response;

import turtleMart.product.entity.ProductOptionValue;

public record ProductOptionValueResponse(
        Long id,
        String name) {
    public static ProductOptionValueResponse from(ProductOptionValue productOptionValue) {
        return new ProductOptionValueResponse(productOptionValue.getId(),productOptionValue.getName());
    }
}
