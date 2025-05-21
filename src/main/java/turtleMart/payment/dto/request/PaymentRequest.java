package turtleMart.payment.dto.request;

import turtleMart.payment.entity.PaymentMethod;

public record PaymentRequest(
        Long orderId,
        Long memberId,
        int amount,
        PaymentMethod paymentMethod,
        String cardCompany,
        int installmentMonth
) {
}
