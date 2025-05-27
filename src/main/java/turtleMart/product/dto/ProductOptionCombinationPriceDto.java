package turtleMart.product.dto;

public record ProductOptionCombinationPriceDto(
        Long productOptionCombinationId,
        Integer price
) {
    public static ProductOptionCombinationPriceDto of(Long productOptionCombinationId, Integer price) {
        return new ProductOptionCombinationPriceDto(productOptionCombinationId,price);
    }
}
