package turtleMart.payment.dto.request;

import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import turtleMart.order.dto.request.OrderRequest;
import turtleMart.order.entity.Order;
import turtleMart.payment.entity.PaymentMethod;

public record PaymentRequest(
        @NotNull Long orderId,
        @NotNull Long memberId,
        @Min(value = 100, message = "100원 이상 결제하셔야 합니다.") int amount,
        @NotNull PaymentMethod paymentMethod,
        @NotBlank String cardCompany,
        int installmentMonth
) {
    public static PaymentRequest from(Order order, Long memberId, OrderRequest request) {
        return new PaymentRequest(
                order.getId(),
                memberId,
                order.getTotalPrice(),
                PaymentMethod.valueOf(request.paymentMethod()),
                request.cardCompany(),
                request.installmentMonth()
        );
    }
}
