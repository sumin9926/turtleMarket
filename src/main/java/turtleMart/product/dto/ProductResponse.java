package turtleMart.product.dto;

import turtleMart.product.entity.Product;

public record ProductResponse(
        Long productId,
        String businessName,
        String name,
        int price,
        String description
) {
    public static ProductResponse from(Product product) {
        return new ProductResponse(product.getId(),null,product.getName(),product.getPrice(), product.getDescription());
    }
}
