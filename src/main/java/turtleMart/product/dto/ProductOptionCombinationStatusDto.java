package turtleMart.product.dto;

import turtleMart.product.entity.CombinationStatus;

public record ProductOptionCombinationStatusDto(
        Long productOptionCombinationId,
        String operationId,
        CombinationStatus combinationStatus
) {
    public static ProductOptionCombinationStatusDto of(Long productOptionCombinationId, String operationId, CombinationStatus combinationStatus) {
        return new ProductOptionCombinationStatusDto(productOptionCombinationId, operationId, combinationStatus);
    }
}
