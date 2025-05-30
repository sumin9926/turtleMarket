package turtleMart.payment.dto.request;

import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import turtleMart.payment.entity.PaymentMethod;

public record PaymentRequest(
        Long orderId, //컨트롤러에서는 null 로 받기, Service 에서 채워줄 예정
        @NotNull Long memberId,
        @Min(value = 100, message = "100원 이상 결제하셔야 합니다.") int amount,
        @NotNull PaymentMethod paymentMethod,
        @NotBlank String cardCompany,
        int installmentMonth
) {
    public static PaymentRequest updateOrderId(PaymentRequest paymentRequest, Long orderId){
        return new PaymentRequest(
                orderId,
                paymentRequest.memberId(),
                paymentRequest.amount(),
                paymentRequest.paymentMethod(),
                paymentRequest.cardCompany(),
                paymentRequest.installmentMonth()
        );
    }
}
