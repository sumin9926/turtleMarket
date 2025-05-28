package turtleMart.global.kafka.consumer;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Component;
import turtleMart.global.exception.ConflictException;
import turtleMart.global.kafka.dto.KafkaMessage;
import turtleMart.global.kafka.dto.InventoryDecreasePayload;
import turtleMart.product.service.ProductOptionCombinationService;

@Slf4j
@Component
@RequiredArgsConstructor
public class ProductConsumer {

    private final ProductOptionCombinationService productOptionCombinationService;
    private final ObjectMapper objectMapper;

    @KafkaListener(topics = "${kafka.topic.stock}", groupId = "${spring.kafka.consumer.group-id}")
    public void listen(ConsumerRecord<String, KafkaMessage<?>> record) {
        KafkaMessage<?> message = record.value();
        log.info("📥 Kafka 메시지 수신: {}", message);

        try {
            switch (message.type()) {
                case "INVENTORY_DECREASE" -> {
                    InventoryDecreasePayload payload = objectMapper.convertValue(
                        message.payload(), InventoryDecreasePayload.class
                    );

                    productOptionCombinationService.decreaseProductOptionCombinationInventory(payload.orderId());
                }
                case "PRICE_CHANGE" -> {

                }
                default -> log.warn("❗ 지원하지 않는 메시지 타입 수신: {}", message.type());
            }
        } catch (ConflictException e) {
            log.warn("⚠️ 재고 부족으로 메시지 처리 실패: {}", e.getMessage());
        }
    }
}
