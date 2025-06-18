package turtleMart.order.dto.response;

import turtleMart.product.entity.Product;
import turtleMart.product.entity.ProductOptionCombination;

public record ResolvedProductOption(
        Product product,
        ProductOptionCombination productOption,
        String optionInfo
) {
}
