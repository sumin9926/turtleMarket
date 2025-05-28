package turtleMart.product.dto.response;

import turtleMart.product.entity.ProductOptionMap;
import turtleMart.product.entity.ProductOptionValue;

public record ProductOptionMapResponse(
        Long valueId,
        String name
) {

    public static ProductOptionMapResponse from(ProductOptionMap productOptionMap) {
        ProductOptionValue productOptionValue = productOptionMap.getProductOptionValue();
        return new ProductOptionMapResponse(productOptionValue.getId(),productOptionValue.getName());
    }
}
