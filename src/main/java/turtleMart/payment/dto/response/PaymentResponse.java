package turtleMart.payment.dto.response;

import turtleMart.payment.entity.Payment;
import turtleMart.payment.entity.PaymentMethod;

import java.time.LocalDateTime;

public record PaymentResponse(
        Long id,
        Long orderId,
        Long memberId,
        String memberName,
        Integer amount,
        PaymentMethod paymentMethod,
        String cardCompany,
        Integer installmentMonth,
        LocalDateTime createdAt
) {
    public static PaymentResponse from(Payment payment) {
        return new PaymentResponse(
                payment.getId(),
                payment.getOrder().getId(),
                payment.getMember().getId(),
                payment.getMember().getName(),
                payment.getTotalAmount(),
                payment.getPaymentMethod(),
                payment.getCardCompany(),
                payment.getInstallmentMonth(),
                payment.getCreatedAt()
        );
    }
}
