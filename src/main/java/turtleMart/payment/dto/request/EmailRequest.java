package turtleMart.payment.dto.request;

import turtleMart.order.entity.Order;
import turtleMart.payment.entity.Payment;

import java.time.format.DateTimeFormatter;

public record EmailRequest(
        String to,
        String subject,
        String content
) {
    public static EmailRequest from(Order order, Payment payment) {
        String orderName = order.getOrderItems().get(0).getName();
        if (order.getOrderItems().size() != 1) {
            orderName += " 외 " + (order.getOrderItems().size() - 1) + "개 상품";
        }

        String formattedDate = payment.getCreatedAt()
                .format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm"));
        return new EmailRequest(
                order.getMember().getEmail(),
                orderName + " 결제 완료 안내 메일입니다.",
                """
                상품명 : %s
                결제 금액 : %d KRW
                결제 일시 : %s""".formatted(orderName, payment.getTotalAmount(), formattedDate)
        );
    }
}
