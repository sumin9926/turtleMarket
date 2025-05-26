package turtleMart.product.dto;

public record ProductOptionCombinationRedisDto(
        Long productCombinationId,
        String operationId,
        Boolean success
) {
}
