package turtleMart.product.dto;

public record ProductOptionCombinationInventoryDto(
        Long productOptionCombinationId,
        Integer inventory
) {
    public static ProductOptionCombinationInventoryDto of(Long productOptionCombinationId, Integer inventory) {
        return new ProductOptionCombinationInventoryDto(productOptionCombinationId, inventory);
    }
}
