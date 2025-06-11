package turtleMart.payment.dto;

import turtleMart.order.entity.Order;

public record PaymentInfoTransfer(
        String orderId,
        Integer totalAmount,
        String orderName,
        String customerEmail,
        String customerName,
        String customerPhoneNumber
) {
    public static PaymentInfoTransfer from(Order order){
        String orderId = "order_" + order.getId();
        String orderName = order.getSummaryItemName();

        return new PaymentInfoTransfer(
                orderId,
                order.getTotalPrice(),
                orderName,
                order.getMember().getEmail(),
                order.getMember().getName(),
                order.getMember().getPhoneNumber()
        );
    }
}
