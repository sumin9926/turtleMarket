package turtleMart.order.dto.request;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;

import java.util.List;

public record OrderRequest(
        @NotNull(message = "장바구니 항목 ID 리스트는 필수입니다.")
        List<Long> cartItemIdList,

        @NotBlank(message = "결제 수단을 입력해주세요.")
        String paymentMethod,

        @NotBlank(message = "카드사를 입력해주세요.")
        String cardCompany,

        int installmentMonth,

        String deliveryRequest,

        @NotNull(message = "주소 ID는 필수입니다.")
        Long addressId
) {
}
