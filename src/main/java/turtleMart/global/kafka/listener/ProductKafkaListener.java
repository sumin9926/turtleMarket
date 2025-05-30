package turtleMart.global.kafka.listener;

import com.fasterxml.jackson.core.type.TypeReference;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.messaging.handler.annotation.Header;
import org.springframework.kafka.support.KafkaHeaders;
import org.springframework.stereotype.Component;
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

    // TODO kafka send 성공 여부 로그로 남기기

    @KafkaListener(topics = "order_make_topic", groupId = "order-group")
    public void listen(@Header(KafkaHeaders.RECEIVED_KEY) String key, String value) {
        try {
            OperationWrapperDto wrapperDto = JsonHelper.fromJson(value, OperationWrapperDto.class);
            OperationType type = wrapperDto.operationType();

            switch (type) { // TODO 각 케이스 내부 로직으로 메서드로 추출해라 (단일 책임 원칙)
                case PRICE_CHANGE -> {
                    String lockKey = "softLock:priceChange:combination:" + key;
                    if (Boolean.FALSE.equals(redisTemplate.hasKey(lockKey))) {
                        // 소프트락이 없으면 걸고 재발송
                        redisTemplate.opsForValue().set(lockKey, false, Duration.ofMinutes(4)); //TODO Duration.ofMinutes(4) 하드코딩 피하기 수정필요하면 매번 찾아서 수정해야하니까 파이널 상수로 빼던가 프로퍼디스에 넘기던가.
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
                default -> log.error("알 수 없는 타입이 입력되었습니다.");
            }
        } catch (Exception e) {// TODO 어떤 오류를 잡을것인지 세분화해라.
            log.error("Kafka message handling error", e);
        }
    }
}
