package turtleMart.order.dto.request;

import turtleMart.delivery.dto.reqeust.CreateDeliveryRequest;
import turtleMart.payment.dto.request.PaymentRequest;

public record PaymentWrapperRequest(
        PaymentRequest payment,
        CreateDeliveryRequest delivery
) {
    public static PaymentWrapperRequest from(PaymentRequest payment, CreateDeliveryRequest delivery){
        return new PaymentWrapperRequest(payment, delivery);
    }
}
