package turtleMart.payment.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.Resource;
import org.springframework.mail.MailSender;
import org.springframework.mail.SimpleMailMessage;
import org.springframework.stereotype.Service;
import turtleMart.order.entity.Order;
import turtleMart.payment.entity.Payment;
import turtleMart.payment.repository.PaymentRepository;

import java.nio.charset.StandardCharsets;
import java.time.format.DateTimeFormatter;

@Slf4j
@Service
@RequiredArgsConstructor
public class EmailService {
    private final MailSender mailSender;
    private final PaymentRepository paymentRepository;
    private static final DateTimeFormatter FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");

    public void sendPaymentCompleteEmail(Long orderId) {
        Payment foundPayment = paymentRepository.findByOrderId(orderId).orElseThrow(
                () -> new RuntimeException(""));
        Order order = foundPayment.getOrder();

        try {
            String summaryItemName = order.getSummaryItemName();
            String formattedDate = foundPayment.getCreatedAt().format(FORMATTER);

            Resource resource = new ClassPathResource("templates/mail/payment-complete_ko.txt");
            String template = new String(resource.getInputStream().readAllBytes(), StandardCharsets.UTF_8);

            String[] parts = template.split("#body");
            String subject = parts[0].replace("#subject", "").trim();
            String body = parts[1].replace("#body", "").trim();

            body = body.replace("{orderName}", summaryItemName)
                    .replace("{amount}", order.getTotalPrice().toString())
                    .replace("{date}", formattedDate);

            SimpleMailMessage message = new SimpleMailMessage();
            message.setTo(order.getMember().getEmail());
            message.setSubject(subject);
            message.setText(body);

            mailSender.send(message);
            log.info("메일 발송 완료: orderId={}, to={}", orderId, order.getMember().getEmail());
        } catch (Exception e) {
            log.error("메일 발송 실패: orderId={}, to={}", orderId, order.getMember().getEmail(), e);
            throw new RuntimeException(e);
        }
    }
}
