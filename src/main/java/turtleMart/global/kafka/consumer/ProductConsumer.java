package turtleMart.global.kafka.consumer;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.stereotype.Component;
import turtleMart.delivery.dto.reqeust.CreateDeliveryRequest;
import turtleMart.global.exception.ConflictException;
import turtleMart.global.kafka.dto.InventoryDecreasePayload;
import turtleMart.global.kafka.dto.KafkaMessage;
import turtleMart.product.service.ProductOptionCombinationService;

@Slf4j
@Component
@RequiredArgsConstructor
public class ProductConsumer {

    private final ProductOptionCombinationService productOptionCombinationService;
    private final ObjectMapper objectMapper;
//    private final DeliveryProducer deliveryProducer;
    private final KafkaTemplate<String, Object> kafkaTemplate;

    @Value("${kafka.topic.delivery}")
    private String deliveryTopic;

    @KafkaListener(topics = "${kafka.topic.stock}", groupId = "${spring.kafka.consumer.group-id}")
    public void listen(ConsumerRecord<String, KafkaMessage<?>> record) {
        KafkaMessage<?> message = record.value();
        log.info("📥 Kafka 재고 감소 메시지 수신: {}", message);

        try {
            switch (message.type()) {
                case "INVENTORY_DECREASE" -> {
                    InventoryDecreasePayload payload = objectMapper.convertValue(
                        message.payload(), InventoryDecreasePayload.class
                    );

                    productOptionCombinationService.decreaseProductOptionCombinationInventory(payload.orderId());

                    log.info("👉 재고 감소 성공! 모든 상품의 재고 차감이 정상적으로 처리되었습니다.");

                    // todo CreateDeliveryRequest 생성 로직 추가 (현재는 임시로 추가)
                    CreateDeliveryRequest request = new CreateDeliveryRequest(
                        payload.orderId(),
                        payload.sellerId(),
                        payload.senderId(),
                        payload.addressId(),
                        payload.deliveryRequest());
                    kafkaTemplate.send(deliveryTopic, request);
                    log.info("\uD83D\uDCE4 Kafka 배송 생성 메시지 전송: {}", request);
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
