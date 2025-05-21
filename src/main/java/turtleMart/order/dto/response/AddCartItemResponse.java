package turtleMart.order.dto.response;

public record AddCartItemResponse(
        Long cartItemId,
        Long productId,
        Long quantity,
        Boolean isChecked
) {
}
