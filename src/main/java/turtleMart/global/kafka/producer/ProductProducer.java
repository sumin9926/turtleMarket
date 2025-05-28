package turtleMart.global.kafka.producer;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.stereotype.Component;
import turtleMart.global.kafka.dto.KafkaMessage;
import turtleMart.global.kafka.dto.InventoryDecreasePayload;

@Slf4j
@Component
@RequiredArgsConstructor
public class ProductProducer {

    private final KafkaTemplate<String, KafkaMessage<InventoryDecreasePayload>> kafkaTemplate;

    @Value("${kafka.topic.stock}")
    private String topic;

    public void sendStockDecreaseMessage(KafkaMessage<InventoryDecreasePayload> message) {
        log.info("Sending stock decrease message: {}", message);
        kafkaTemplate.send(topic, message.payload().orderId().toString(), message);
    }
}
