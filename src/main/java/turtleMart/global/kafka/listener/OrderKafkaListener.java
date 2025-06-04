package turtleMart.global.kafka.listener;

import lombok.RequiredArgsConstructor;
import org.springframework.kafka.annotation.KafkaListener;
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

    private final OrderService orderService;

    @KafkaListener(topics = "order_create_topic", groupId = "order-group")
    public void listenOrderCreate(@Header(KafkaHeaders.RECEIVED_KEY) String key, String value){
        OperationWrapperDto wrapperDto = JsonHelper.fromJson(value, OperationWrapperDto.class);
        String payload = wrapperDto.payload();
        OrderWrapperRequest orderWrapperRequest = JsonHelper.fromJson(payload, OrderWrapperRequest.class);
        // TODO 파싱, 서비스 호출 안 됐을 때 try-catch 예외 추가, 로그 추가 혹은 재시도 전략 고민


        orderService.createOrder(orderWrapperRequest);
    }
}
