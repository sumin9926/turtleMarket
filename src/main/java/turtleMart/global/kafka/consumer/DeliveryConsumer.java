package turtleMart.global.kafka.consumer;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Component;
import turtleMart.delivery.dto.reqeust.CreateDeliveryRequest;
import turtleMart.delivery.service.DeliveryService;

@Slf4j
@Component
@RequiredArgsConstructor
public class DeliveryConsumer {

    private final DeliveryService deliveryService;


    @KafkaListener(topics = "${kafka.topic.delivery}", groupId = "${spring.kafka.consumer.delivery.group-id}")
    public void listen(ConsumerRecord<String, CreateDeliveryRequest> record) {
        CreateDeliveryRequest request = record.value();
        log.info("📥 Kafka 배송 생성 메시지 수신: {}", request);

        deliveryService.createDelivery(request);

        log.info("👉 배송 생성이 정상적으로 처리되었습니다.");
    }
}
