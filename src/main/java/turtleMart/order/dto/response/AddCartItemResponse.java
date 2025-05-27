package turtleMart.order.dto.response;

public record AddCartItemResponse(
        Long cartItemId,
        Long productOptionId, //==productOptionCombinationId
        Integer quantity,
        Boolean isChecked
) {
}
