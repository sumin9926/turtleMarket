package turtleMart.global.redis.redisPubSub;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.connection.Message;
import org.springframework.data.redis.connection.MessageListener;
import org.springframework.stereotype.Component;
import turtleMart.global.utill.JsonHelper;
import turtleMart.order.dto.request.OrderWrapperRequest;
import turtleMart.order.service.OrderWaiter;

import java.nio.charset.StandardCharsets;

@Slf4j
@Component
@RequiredArgsConstructor
public class OrderRedisSubscriber implements MessageListener {

    private final OrderWaiter orderWaiter;

    @Override
    public void onMessage(Message message, byte[] pattern) {
        String jsonMessage = new String(message.getBody(), StandardCharsets.UTF_8);
        OrderWrapperRequest request = JsonHelper.fromJson(jsonMessage, OrderWrapperRequest.class);
        String orderKey = request.orderKey();
        log.info("Redis 메시지 수신 - orderKey: {}", orderKey);
        orderWaiter.completeOrder(orderKey, request);
    }
}
