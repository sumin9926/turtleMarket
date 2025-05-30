package turtleMart.payment.dto;

import turtleMart.order.entity.Order;

public record PaymentInfoTransfer(
        Long orderId,
        Integer totalAmount,
        String orderName,
        String customerEmail,
        String customerName,
        String customerPhoneNumber
) {
    public static PaymentInfoTransfer from(Order order){
        String orderName = order.getOrderItems().get(0).getName();
        if (order.getOrderItems().size() != 1) {
            orderName += " 외 " + (order.getOrderItems().size() - 1) + "개 상품";
        }
        return new PaymentInfoTransfer(
                order.getId(),
                order.getTotalPrice(),
                orderName,
                order.getMember().getEmail(),
                order.getMember().getName(),
                order.getMember().getPhoneNumber()
        );
    }
}
