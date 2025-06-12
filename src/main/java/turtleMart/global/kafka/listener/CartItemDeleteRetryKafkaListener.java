package turtleMart.global.kafka.listener;

import io.lettuce.core.RedisCommandTimeoutException;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.RedisConnectionFailureException;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.stereotype.Component;
import turtleMart.global.kafka.util.KafkaSendHelper;
import turtleMart.order.dto.request.CartItemDeleteRetryMessage;

@Slf4j
@RequiredArgsConstructor
@Component
public class CartItemDeleteRetryKafkaListener {

    @Value("${kafka.topic.delete.cart-item.with.delay}")
    private String deleteCartItemWithDelayTopic;

    @Value("${kafka.topic.delete.cart-item}")
    private String deleteCartItemTopic;

    private final KafkaSendHelper kafkaSendHelper;
    private final RedisTemplate<String, String> redisTemplate;
    private final KafkaTemplate<String, Object> objectKafkaTemplate;
    private static final Integer DELAY_TIME = 5000;
    private static final Integer MAXIMUM_ATTEMPT_NUM = 2; //최대 재시도 횟수

    @KafkaListener(topics = "${kafka.topic.delete.cart-item}", groupId = "cart-retry")
    public void cartItemDeleteRetry(CartItemDeleteRetryMessage message) throws InterruptedException {

        Thread.sleep(DELAY_TIME); // 5초 뒤에 재시도 토픽으로 전송

        kafkaSendHelper.sendWithCallback(objectKafkaTemplate, deleteCartItemWithDelayTopic, message.key(),message);
    }

    @KafkaListener(topics = "${kafka.topic.delete.cart-item.with.delay}", groupId = "cart-retry")
    public void cartItemDeleteRetryWithDelay(CartItemDeleteRetryMessage message) {
        String key = message.key();
        String cartItemId = message.cartItemId();

        try {
            redisTemplate.opsForHash().delete(key, cartItemId);
            log.info("재시도 성공: cartItemId={}", cartItemId);
        } catch (RedisConnectionFailureException | RedisCommandTimeoutException e) {
            if (message.attempt() < MAXIMUM_ATTEMPT_NUM) {
                log.warn("재시도 1차 실패, 최종 시도 예정: cartItemId={}",cartItemId);
                CartItemDeleteRetryMessage newMessage = CartItemDeleteRetryMessage.incrementAttempt(message);
                kafkaSendHelper.sendWithCallback(objectKafkaTemplate, deleteCartItemTopic, newMessage.key(), newMessage);
            } else {
                log.warn("최대 재시도 횟수 초과: cartItemId={}", cartItemId);
            }
        }
    }
}
