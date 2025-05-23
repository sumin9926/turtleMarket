package turtleMart.order.dto.response;

public record AddCartItemResponse(
        Long cartItemId,
        Long productId,
        Integer quantity,
        Boolean isChecked
) {
}
