package turtleMart.product.dto;

import turtleMart.global.common.OperationType;

public record ProductOptionCombinationRedisDto(
        Long productCombinationId,
        String operationId,
        Boolean success,
        OperationType operationType
) {
}
