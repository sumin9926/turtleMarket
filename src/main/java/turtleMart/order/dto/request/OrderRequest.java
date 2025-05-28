package turtleMart.order.dto.request;

import jakarta.validation.constraints.NotNull;

public record OrderRequest(
        @NotNull(message = "장바구니 항목 ID 리스트는 필수입니다.")
        Long cartItemId,

        Long productOptionId,

        @NotNull(message = "단품 가격 입력은 필수입니다.")
        Integer price, // 주문서 가격을 그대로 스냅샷으로 들고와야한다.

        @NotNull(message = "전체 가격는 필수입니다.")
        Integer totalPrice
) {
}
