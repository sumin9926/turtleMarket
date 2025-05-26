package turtleMart.product.dto.request;

public record ProductRequest(
        String name,
        int price,
        String description
) {
}
