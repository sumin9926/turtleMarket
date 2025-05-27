package turtleMart.order.dto.response;

public record CartItemResponse(
        Long cartItemId,
        Long productId,
        Long productOptionId, //==productOptionCombinationId
        String productName,
        String optionInfo, //옵션상세(productOptionCombination 의 uniqueKey 필드에 해당, 대신 유저가 읽기 좋은 형식으로 변환됨)
        Integer productPrice,
        Integer quantity,
        Boolean isChecked
) {
}
