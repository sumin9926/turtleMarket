package turtleMart.global.kafka.producer;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.stereotype.Component;
import turtleMart.delivery.dto.reqeust.CreateDeliveryRequest;

@Slf4j
@Component
@RequiredArgsConstructor
public class DeliveryProducer {

    private final KafkaTemplate<String, Object> kafkaTemplate;

    @Value("${kafka.topic.delivery}")
    private String deliveryTopic;

    public void send(CreateDeliveryRequest request) {
        log.info("\uD83D\uDCE4 Kafka 배송 생성 메시지 전송: {}", request);
        kafkaTemplate.send(deliveryTopic, request);
    }
}
