package turtleMart.global.redis.redisPubSub;

import lombok.RequiredArgsConstructor;
import org.springframework.data.redis.connection.Message;
import org.springframework.data.redis.connection.MessageListener;
import org.springframework.stereotype.Component;
import turtleMart.order.service.RefundWaiter;

import java.nio.charset.StandardCharsets;

@Component
@RequiredArgsConstructor
public class RefundRedisSubscriber implements MessageListener {

    private final RefundWaiter refundWaiter;

    @Override
    public void onMessage(Message message, byte[] pattern) {
        String orderItemIdStr = new String(message.getBody(), StandardCharsets.UTF_8);
        Long orderItemId = Long.valueOf(orderItemIdStr);
        refundWaiter.completeRefund(orderItemId); // 요청 온 서버(=WaitMap에 Id가 저장되어있는 서버)에서만 응답됨
    }
}
