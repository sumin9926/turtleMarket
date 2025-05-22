package turtleMart.order.dto.response;

public record OrderSheetResponse(
        Long productId,
        String productName,
        Integer productPrice,
        Integer quantity
) {
}
