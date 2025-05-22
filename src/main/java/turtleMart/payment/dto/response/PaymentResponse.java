package turtleMart.payment.dto.response;

import turtleMart.member.entity.Member;
import turtleMart.payment.entity.FailReason;
import turtleMart.payment.entity.Payment;
import turtleMart.payment.entity.PaymentMethod;
import turtleMart.payment.entity.PaymentStatus;

import java.time.LocalDateTime;

public record PaymentResponse(
        Long id,
        Long orderId,
        Long memberId,
        String memberName,
        int amount,
        PaymentMethod paymentMethod,
        String cardCompany,
        int installmentMonth,
        PaymentStatus paymentStatus,
        FailReason failReason,
        LocalDateTime createdAt
) {
    public static PaymentResponse from(Payment payment) {
        return new PaymentResponse(
                payment.getId(),
                payment.getOrder().getId(),
                payment.getMember().getId(),
                payment.getMember().getName(),
                payment.getAmount(),
                payment.getPaymentMethod(),
                payment.getCardCompany(),
                payment.getInstallmentMonth(),
                payment.getPaymentStatus(),
                payment.getFailReason(),
                payment.getCreatedAt()
        );
    }
}
