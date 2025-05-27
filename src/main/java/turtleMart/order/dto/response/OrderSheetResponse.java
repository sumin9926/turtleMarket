package turtleMart.order.dto.response;

public record OrderSheetResponse(
        Long productOptionId, //==productOptionCombinationId
        Long productId,
        String optionInfo,
        String productName,
        Integer productPrice,
        Integer quantity
) {
}
