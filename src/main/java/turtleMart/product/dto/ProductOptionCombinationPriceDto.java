package turtleMart.product.dto;

public record ProductOptionCombinationPriceDto(
        Long productOptionCombinationId,
        Integer price,
        String operationId
) {
    public static ProductOptionCombinationPriceDto of(Long productOptionCombinationId, Integer price, String operationId) {
        return new ProductOptionCombinationPriceDto(productOptionCombinationId,price,operationId);
    }
}
