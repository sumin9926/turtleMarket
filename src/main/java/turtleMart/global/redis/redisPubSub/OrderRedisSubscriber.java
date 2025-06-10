package turtleMart.global.redis.redisPubSub;

import lombok.RequiredArgsConstructor;
import org.springframework.data.redis.connection.Message;
import org.springframework.data.redis.connection.MessageListener;
import org.springframework.stereotype.Component;
import turtleMart.global.utill.JsonHelper;
import turtleMart.order.dto.request.OrderWrapperRequest;
import turtleMart.order.service.OrderWaiter;

import java.nio.charset.StandardCharsets;

@Component
@RequiredArgsConstructor
public class OrderRedisSubscriber implements MessageListener {

    private final OrderWaiter orderWaiter;

    @Override
    public void onMessage(Message message, byte[] pattern) {
        String jsonMessage = new String(message.getBody(), StandardCharsets.UTF_8); // byte[] -> String 변환
        OrderWrapperRequest request = JsonHelper.fromJson(jsonMessage, OrderWrapperRequest.class);
        String orderKey = request.orderKey();
        orderWaiter.completeOrder(orderKey, request);
    }
}
