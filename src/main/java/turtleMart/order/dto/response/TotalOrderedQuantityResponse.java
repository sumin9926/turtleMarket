package turtleMart.order.dto.response;

public record TotalOrderedQuantityResponse(
        Long productId,
        Long totalOrdered
) {
    public static TotalOrderedQuantityResponse from(Long productId, Long totalOrdered){
        return new TotalOrderedQuantityResponse(productId, totalOrdered);
    }
}
