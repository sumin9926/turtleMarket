package turtleMart.order.dto.response;

import turtleMart.product.entity.Product;
import turtleMart.product.entity.ProductOptionCombination;

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
    public static CartItemResponse from(
            AddCartItemResponse addCartItemResponse, Product product,
            ProductOptionCombination optionCombination, String optionInfo
    ) {
        return new CartItemResponse(
                addCartItemResponse.cartItemId(),
                product.getId(),
                optionCombination.getId(),
                product.getName(),
                optionInfo,
                optionCombination.getPrice(),
                addCartItemResponse.quantity(),
                addCartItemResponse.isChecked()
        );
    }
}
