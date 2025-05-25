package turtleMart.product.dto.response;

import java.util.List;

public record ProductOptionCombinationResponseCreate(
        List<ProductOptionCombinationResponse> productOptionCombinationResponseList,
        DuplicateList duplicateList
) {
    public static ProductOptionCombinationResponseCreate of(List<ProductOptionCombinationResponse> productOptionCombinationResponseList, DuplicateList duplicateList) {
        return new ProductOptionCombinationResponseCreate(
                productOptionCombinationResponseList,
                duplicateList
        );
    }
}
