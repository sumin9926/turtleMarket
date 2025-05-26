package turtleMart.product.dto.response;

import turtleMart.product.entity.ProductOptionCombination;

import java.util.List;

public record ProductOptionCombinationResponse(
        Long id,
        Integer price,
        Integer inventory,
        List<ProductOptionMapResponse> productOptionMapResponseList
) {
    public static ProductOptionCombinationResponse from(ProductOptionCombination productOptionCombination) {
        return new ProductOptionCombinationResponse(
                productOptionCombination.getId(),
                productOptionCombination.getPrice(),
                productOptionCombination.getInventory(),
                productOptionCombination.getProductOptionMapList().stream().map(ProductOptionMapResponse::from).toList()
        );
    }
}
