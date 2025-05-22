package turtleMart.order.dto.response;

public record TotalOrderedQuantityResponse(
        Long productId,
        Integer totalOrdered
) {
    public static TotalOrderedQuantityResponse from(Long productId, Integer totalOrdered){
        return new TotalOrderedQuantityResponse(productId, totalOrdered);
    }
}
