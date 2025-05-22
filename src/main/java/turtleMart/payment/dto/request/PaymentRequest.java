package turtleMart.payment.dto.request;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;
import turtleMart.payment.entity.PaymentMethod;

public record PaymentRequest(
        @NotBlank Long orderId,
        @NotBlank Long memberId,
        @NotBlank @Size(min = 100, message = "100원 이상 결제하셔야 합니다.") int amount,
        @NotBlank PaymentMethod paymentMethod,
        @NotBlank String cardCompany,
        int installmentMonth
) {
}
