package turtleMart.global.kafka.listener;

import io.lettuce.core.RedisCommandTimeoutException;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.RedisConnectionFailureException;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.stereotype.Component;
import turtleMart.order.dto.request.CartItemDeleteRetryMessage;

@Slf4j
@RequiredArgsConstructor
@Component
public class CartItemDeleteRetryWithDelayKafkaListener {

    private final RedisTemplate<String, String> redisTemplate;
    private final KafkaTemplate<String, Object> objectKafkaTemplate;
    private static final String KAFKA_DELETE_CART_ITEM_TOPIC = "delete_cart_item_topic"; //TODO properties 로 이동 시키기

    @KafkaListener(topics = "delete_cart_item_retry_with_delay_topic", groupId = "cart-retry")
    public void cartItemDeleteRetryWithDelay(CartItemDeleteRetryMessage message) {
        String key = message.key();
        String cartItemId = message.cartItemId();

        try {
            redisTemplate.opsForHash().delete(key, cartItemId);
            log.info("재시도 성공: cartItemId={}", cartItemId);
        } catch (RedisConnectionFailureException | RedisCommandTimeoutException e) {
            if (message.attempt() < 2) {
                log.warn("재시도 1차 실패, 최종 시도 예정: cartItemId={}",cartItemId);
                CartItemDeleteRetryMessage newMessage = CartItemDeleteRetryMessage.incrementAttempt(message);
                objectKafkaTemplate.send(KAFKA_DELETE_CART_ITEM_TOPIC, newMessage); //log 찍기
            } else {
                log.warn("최대 재시도 횟수 초과: cartItemId={}", cartItemId);
            }
        }
    }
}
