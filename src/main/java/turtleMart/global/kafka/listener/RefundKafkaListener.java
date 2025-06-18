package turtleMart.global.kafka.listener;

import lombok.RequiredArgsConstructor;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Component;

import java.util.concurrent.TimeUnit;

@Component
@RequiredArgsConstructor
public class RefundKafkaListener {

    private final RedisTemplate<String, Object> redisTemplate;

    @KafkaListener(topics = "${kafka.topic.refund.approve}")
    public void onRefundApprove(String message) {
        Long orderItemId = Long.parseLong(message);

        // 1. 환불 처리 및 orderItemStatus를 REFUNDED로 변경


        // 2. Redis 상태 true로 변경(=환불 처리 정상적으로 완료 됨)
        redisTemplate.opsForValue().set("refund:status:" + orderItemId, true, 1, TimeUnit.MINUTES);
        redisTemplate.convertAndSend("refund:completed", Long.toString(orderItemId)); //채널에 메세지 발행(브로드케스팅)
    }
}
