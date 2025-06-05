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
import turtleMart.global.kafka.dto.OperationWrapperDto;
import turtleMart.global.kafka.enums.OperationType;
import turtleMart.global.utill.JsonHelper;

import java.time.Duration;
import java.util.List;

@RequiredArgsConstructor
@Component
@Slf4j
public class ProductKafkaListener {

    private final RedisTemplate<String, Object> redisTemplate;
    private final KafkaTemplate<String, String> kafkaTemplate;

    @KafkaListener(topics = "order_make_topic", groupId = "order-group")
    public void listen(@Header(KafkaHeaders.RECEIVED_KEY) String key, String value) {
        try {
            OperationWrapperDto wrapperDto = JsonHelper.fromJson(value, OperationWrapperDto.class);
            OperationType type = wrapperDto.operationType();

            switch (type) {
                case PRICE_CHANGE -> {
                    String lockKey = "softLock:priceChange:combination:" + key;
                    if (Boolean.FALSE.equals(redisTemplate.hasKey(lockKey))) {
                        // 소프트락이 없으면 걸고 재발송
                        redisTemplate.opsForValue().set(lockKey, false, Duration.ofMinutes(4));
                        Thread.sleep(1000);
                        kafkaTemplate.send("order_make_topic", key, value);
                    } else {
                        // 소프트락이 이미 존재하면 가격변동 처리용 새 토픽에 발행
                        kafkaTemplate.send("", key, value);/*TODO 토픽 이름 넣기 */
                    }
                }
                case ORDER_CREATE -> {
                    //key 역직렬화 및 가격 변경중인지 확인
                    List<Long> productOptionCombinationIdList = JsonHelper.fromJsonToList(key, new TypeReference<>() {});
                    // 주문 요청 상품 중 하나라도 가격 변경 중이라면 주문 생성 중단
                    for(Long combinationId : productOptionCombinationIdList){
                        String lockKey = "softLock:priceChange:combination:" + combinationId.toString();
                        if(Boolean.TRUE.equals(redisTemplate.opsForValue().get(lockKey))){ //가격 변동 처리 중 상태(Redis value=true)
                            kafkaTemplate.send("order_make_topic", key, value); //재발행
                            return;
                        }
                    }
                    // 가격 변동이 끝났다면 주문 생성 로직 실행, 주문 생성 토픽으로 넘기기
                    kafkaTemplate.send("order_create_topic", key, value);
                }
                case INVENTORY_DECREASE -> {
                    // todo 결제 파트에서 전달되는 payload 구조 확인 후 DTO 정의
                    // log.info("📥 Kafka 재고 감소 메시지 수신: {}", message);

                    // 재고 감소 로직 진행
                    // productOptionCombinationService.decreaseProductOptionCombinationInventory(payload.orderId());
                    // log.info("👉 재고 감소 성공! 모든 상품의 재고 차감이 정상적으로 처리되었습니다.");

                    // todo 배송 생성 메시지 발행
                    // kafkaTemplate.send(deliveryTopic, request);
                    // log.info("\uD83D\uDCE4 Kafka 배송 생성 메시지 전송: {}", request);
                }
                default -> log.error("❗ 지원하지 않는 메시지 타입 수신: {}", type);
            }
        } catch (ConflictException e) {
            log.warn("⚠️ 재고 부족으로 메시지 처리 실패: {}", e.getMessage());
        } catch (Exception e) {
            log.error("Kafka message handling error", e);
        }
    }
}
