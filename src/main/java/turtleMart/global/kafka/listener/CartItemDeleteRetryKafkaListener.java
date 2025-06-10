package turtleMart.global.kafka.listener;

import lombok.RequiredArgsConstructor;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.stereotype.Component;
import turtleMart.order.dto.request.CartItemDeleteRetryMessage;

@RequiredArgsConstructor
@Component
public class CartItemDeleteRetryKafkaListener {

    private final KafkaTemplate<String, Object> objectKafkaTemplate;
    private static final String KAFKA_DELETE_CART_ITEM_RETRY_WITH_DELAY_TOPIC = "delete_cart_item_retry_with_delay_topic"; //TODO properties 로 이동 시키기

    @KafkaListener(topics = "delete_cart_item_topic", groupId = "cart-retry")
    public void cartItemDeleteRetry(CartItemDeleteRetryMessage message) throws InterruptedException {

        Thread.sleep(5000); // 5초 뒤에 재시도 토픽으로 전송

        objectKafkaTemplate.send(KAFKA_DELETE_CART_ITEM_RETRY_WITH_DELAY_TOPIC, message);
    }
}
