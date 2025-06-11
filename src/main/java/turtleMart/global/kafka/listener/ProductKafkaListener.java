package turtleMart.global.kafka.listener;

import com.fasterxml.jackson.core.type.TypeReference;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.kafka.support.KafkaHeaders;
import org.springframework.messaging.handler.annotation.Header;
import org.springframework.stereotype.Component;
import turtleMart.global.exception.ConflictException;
import turtleMart.global.exception.NotFoundException;
import turtleMart.global.kafka.dto.OperationWrapperDto;
import turtleMart.global.kafka.enums.OperationType;
import turtleMart.global.utill.JsonHelper;
import turtleMart.product.service.ProductOptionCombinationService;

import java.time.Duration;
import java.util.List;

@RequiredArgsConstructor
@Component
@Slf4j
public class ProductKafkaListener {

    private final RedisTemplate<String, Object> redisTemplate;
    private final KafkaTemplate<String, String> kafkaTemplate;
    private final ProductOptionCombinationService productOptionCombinationService;

    //TODO 추후에 @Value + SpEL 표현식으로 변경하기 (ex) "${kafka.topics.order-make}")
    private static final String KAFKA_ORDER_MAKE_TOPIC = "order_make_topic";
    private static final String KAFKA_ORDER_CREATE_TOPIC = "order_create_topic";
    private static final String KAFKA_PRICE_CHANGE_TOPIC = "_topic"; //TODO 가격 변경 토픽 이름 정해지면 넣어주세요.
    private static final Duration DURATION_MINUTES = Duration.ofMinutes(4);
    private static final long RETRY_DELAY_MS = 1000L;


    @KafkaListener(topics = "order_make_topic", groupId = "order-group")
    public void listen(@Header(KafkaHeaders.RECEIVED_KEY) String key, String value) {
        try {
            OperationWrapperDto wrapperDto = JsonHelper.fromJson(value, OperationWrapperDto.class);
            OperationType type = wrapperDto.operationType();

            switch (type) {
                case PRICE_CHANGE -> routePriceChangeMessage(key, value);
                case ORDER_CREATE -> routeOrderMessage(key, value);
                default -> log.error("❗ 지원하지 않는 메시지 타입 수신: {}", type);
            }
        } catch (IllegalArgumentException e) {
            log.error("알 수 없는 OperationType 입니다. key={}, value={}", key, value, e);
        } catch (Exception e) {
            log.error("Kafka message 처리 중 알 수 없는 오류 발생", e);
        }
    }

    // 재고 관련 Kafka Listener
    @KafkaListener(topics = "${kafka.topic.product}", groupId = "${spring.kafka.consumer.product.inventory.group-id}")
    public void listenInventory(@Header(KafkaHeaders.RECEIVED_KEY) String key, String value) {
        try {
            OperationWrapperDto wrapperDto = JsonHelper.fromJson(value, OperationWrapperDto.class);
            OperationType type = wrapperDto.operationType();

            switch (type) {
                case ORDER_PAYMENT_INVENTORY_DECREASE -> routeInventoryDecreaseMessage(key, value);
                case DELIVERY_FAIL_INVENTORY_RESTORE -> routeInventoryRestoreMessage(key, value);
                default -> log.error("❌ 지원하지 않는 메시지 타입 수신: {}", type);
            }
        } catch (ConflictException e) {
            log.warn("⚠️ 재고 부족으로 메시지 처리 실패: {}", e.getMessage());
        } catch (NotFoundException e) {
            log.warn("⚠️ 필수 데이터 누락으로 메시지 처리 실패 ({}): {}", e.getErrorCode(), e.getMessage());
        } catch (NumberFormatException e) {
            log.warn("⚠️ 잘못된 orderId 형식입니다. key: {}, message: {}", key, e.getMessage());
        } catch (Exception e) {
            log.error("❌ 예기치 못한 오류로 메시지 처리 실패: {}", e.getMessage());
        }
    }

    private void routePriceChangeMessage(String key, String value) {
        String lockKey = "softLock:priceChange:combination:" + key;
        if (Boolean.FALSE.equals(redisTemplate.hasKey(lockKey))) {
            // 소프트락이 없으면 걸고 재발송
            redisTemplate.opsForValue().set(lockKey, false, DURATION_MINUTES);

            try {
                Thread.sleep(RETRY_DELAY_MS);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt(); // 인터럽트 신호 복원
                log.error("Thread sleep 중 인터럽트 발생", e);
                log.warn("가격 변경 kafka 메세지 전송에 실패 했습니다. key={}, value={}", key, value);
                return;
            }

            kafkaTemplate.send(KAFKA_ORDER_MAKE_TOPIC, key, value);
            log.info("주문 생성 요청 토픽으로 kafka 메세지 재발행 성공! TopicName: {}", KAFKA_ORDER_MAKE_TOPIC);
        } else {
            // 소프트락이 이미 존재하면 가격변동 처리용 새 토픽에 발행
            kafkaTemplate.send(KAFKA_PRICE_CHANGE_TOPIC, key, value);
            log.info("가격 변동 처리 토픽으로 kafka 메세지 전송 성공! TopicName: {}", KAFKA_PRICE_CHANGE_TOPIC);
        }
    }

    private void routeOrderMessage(String key, String value) {
        //key 역직렬화 및 가격 변경중인지 확인
        List<Long> productOptionCombinationIdList = JsonHelper.fromJsonToList(key, new TypeReference<>() {
        });
        // 주문 요청 상품 중 하나라도 가격 변경 중이라면 주문 생성 중단
        for (Long combinationId : productOptionCombinationIdList) {
            String lockKey = "softLock:priceChange:combination:" + combinationId.toString();
            if (Boolean.TRUE.equals(redisTemplate.opsForValue().get(lockKey))) { //가격 변동 처리 중 상태(Redis value=true) (*처리 완료되면 삭제됨, true는 처리 중을 의미)
                kafkaTemplate.send(KAFKA_ORDER_MAKE_TOPIC, key, value); //재발행
                log.info("주문 생성 요청 토픽에 kafka 메세지 재발행 성공! TopicName: {}", KAFKA_ORDER_MAKE_TOPIC);
                return;
            }
        }
        // 가격 변동이 끝났다면 주문 생성 로직 실행, 주문 생성 토픽으로 넘기기
        kafkaTemplate.send(KAFKA_ORDER_CREATE_TOPIC, key, value);
        log.info("주문 생성 요청 처리 토픽으로 kafka 메세지 전송 성공! TopicName: {}", KAFKA_ORDER_CREATE_TOPIC);
    }

    private void routeInventoryDecreaseMessage(String key, String value) {
        // todo 결제 파트에서 전달되는 payload 구조 확인 후 DTO 정의
        log.info("📥 Kafka 재고 감소 메시지 수신: key: {}, value: {}", key, value);

        // 재고 감소 로직 진행
        // productOptionCombinationService.decreaseProductOptionCombinationInventory(payload.orderId());
        // log.info("👉 재고 감소 성공! 모든 상품의 재고 차감이 정상적으로 처리되었습니다.");

        // todo 배송 생성 메시지 발행
        // kafkaTemplate.send(deliveryTopic, request);
        // log.info("\uD83D\uDCE4 Kafka 배송 생성 메시지 전송: {}", request);
    }

    private void routeInventoryRestoreMessage(String key, String value) {
        log.info("📥 Kafka 재고 복원 메시지 수신: key={}, value={}", key, value);

        // 재고 복원 로직 진행
        Long orderId = Long.valueOf(key);
        productOptionCombinationService.restoreProductOptionCombinationInventory(orderId);
    }
}