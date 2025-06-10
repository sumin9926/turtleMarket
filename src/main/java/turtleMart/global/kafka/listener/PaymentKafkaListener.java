package turtleMart.global.kafka.listener;

import lombok.RequiredArgsConstructor;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Component;
import turtleMart.payment.controller.WidgetController;
import turtleMart.payment.service.PaymentService;

@Component
@RequiredArgsConstructor
public class PaymentKafkaListener {

    private final PaymentService paymentService;
    private final WidgetController widgetController;

    @KafkaListener(topics = "payment-topic", groupId = "payment-group")
    public void listen(ConsumerRecord<String, String> record) {}
}
