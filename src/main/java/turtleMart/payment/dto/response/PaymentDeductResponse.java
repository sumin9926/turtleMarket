package turtleMart.payment.dto.response;

import turtleMart.payment.entity.Payment;
import turtleMart.payment.entity.PaymentMethod;

import java.time.LocalDateTime;

public record PaymentDeductResponse(
   Long id,
   Long orderId,
   Long memberId,
   String memberName,
   Integer totalAmount,
   Integer deductedAmount,
   Integer actualAmount,
   PaymentMethod paymentMethod,
   String cardCompany,
   Integer installmentMouth,
   LocalDateTime createdAt,
   LocalDateTime updatedAt
) {
    public static PaymentDeductResponse from(Payment payment) {
        return new PaymentDeductResponse(
                payment.getId(),
                payment.getOrder().getId(),
                payment.getMember().getId(),
                payment.getMember().getName(),
                payment.getTotalAmount(),
                payment.getDeductedAmount(),
                payment.getActualAmount(),
                payment.getPaymentMethod(),
                payment.getCardCompany(),
                payment.getInstallmentMonth(),
                payment.getCreatedAt(),
                payment.getUpdatedAt()
        );
    }
}
