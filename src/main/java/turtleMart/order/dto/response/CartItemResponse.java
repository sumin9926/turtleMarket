package turtleMart.order.dto.response;

public record CartItemResponse(
        Long cartItemId,
        Long productId,
        String productName,
        Integer productPrice,
        Integer quantity,
        Boolean isChecked
) {
}
