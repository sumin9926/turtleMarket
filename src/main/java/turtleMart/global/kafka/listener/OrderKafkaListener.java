package turtleMart.global.kafka.listener;

import lombok.RequiredArgsConstructor;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.kafka.support.KafkaHeaders;
import org.springframework.messaging.handler.annotation.Header;
import org.springframework.stereotype.Component;
import turtleMart.global.kafka.dto.OperationWrapperDto;
import turtleMart.global.utill.JsonHelper;
import turtleMart.order.dto.request.OrderWrapperRequest;
import turtleMart.order.service.OrderService;

@RequiredArgsConstructor
@Component
public class OrderKafkaListener {

    private final RedisTemplate<String, Object> redisTemplate;
    private final KafkaTemplate<String, String> kafkaTemplate;
    private final OrderService orderService;

    @KafkaListener(topics = "order_create_topic", groupId = "order-group")
    public void listenOrderCreate(@Header(KafkaHeaders.RECEIVED_KEY) String key, String value){
        OperationWrapperDto wrapperDto = JsonHelper.fromJson(value, OperationWrapperDto.class);
        String payload = wrapperDto.payload();
        OrderWrapperRequest orderWrapperRequest = JsonHelper.fromJson(payload, OrderWrapperRequest.class);

        orderService.createOrder(orderWrapperRequest);
    }
}
