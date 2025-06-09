package turtleMart.payment.service;

import lombok.RequiredArgsConstructor;
import org.springframework.mail.MailSender;
import org.springframework.mail.SimpleMailMessage;
import org.springframework.stereotype.Service;
import turtleMart.order.entity.Order;
import turtleMart.order.repository.OrderRepository;
import turtleMart.payment.entity.Payment;
import turtleMart.payment.repository.PaymentRepository;

import java.time.format.DateTimeFormatter;

@Service
@RequiredArgsConstructor
public class EmailService {
    private final MailSender mailSender;
    private final OrderRepository orderRepository;
    private final PaymentRepository paymentRepository;

    public void sendPaymentCompleteEmail(Long orderId) {
        Order foundOrder = orderRepository.findById(orderId).orElseThrow(
                () -> new RuntimeException(""));
        Payment foundPayment = paymentRepository.findByOrderId(orderId).orElseThrow(
                () -> new RuntimeException(""));

        String orderName = foundOrder.getOrderItems().get(0).getName();
        if (foundOrder.getOrderItems().size() != 1) {
            orderName += " 외 " + (foundOrder.getOrderItems().size() - 1) + "개 상품";
        }

        String formattedDate = foundPayment.getCreatedAt()
                .format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm"));

        SimpleMailMessage message = new SimpleMailMessage();
        message.setTo(foundOrder.getMember().getEmail());
        message.setSubject("[터틀마트] 결제 완료 안내 메일입니다");
        message.setText(
                """
                상품명 : %s
                결제 금액 : %d KRW
                결제 일시 : %s""".formatted(orderName, foundPayment.getTotalAmount(), formattedDate)
        );

        mailSender.send(message);
    }
}
