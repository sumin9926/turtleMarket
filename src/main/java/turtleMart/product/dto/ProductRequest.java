package turtleMart.product.dto;

public record ProductRequest(
        String name,
        int price,
        String description
) {
}
