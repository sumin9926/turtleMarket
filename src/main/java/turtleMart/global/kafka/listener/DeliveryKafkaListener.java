package turtleMart.global.kafka.listener;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.kafka.support.KafkaHeaders;
import org.springframework.messaging.handler.annotation.Header;
import org.springframework.stereotype.Component;
import turtleMart.delivery.dto.reqeust.CreateDeliveryRequest;
import turtleMart.delivery.service.DeliveryService;
import turtleMart.global.exception.NotFoundException;
import turtleMart.global.kafka.dto.OperationWrapperDto;
import turtleMart.global.utill.JsonHelper;

@Slf4j
@Component
@RequiredArgsConstructor
public class DeliveryKafkaListener {

    private final DeliveryService deliveryService;

    @KafkaListener(topics = "${kafka.topic.delivery}", groupId = "${spring.kafka.consumer.delivery.group-id}")
    public void listenDeliveryCreate(@Header(KafkaHeaders.RECEIVED_KEY) String key, String value) {
        // 메시지 전체를 OperationWrapperDto로 파싱
        OperationWrapperDto wrapperDto = JsonHelper.fromJson(value, OperationWrapperDto.class);

        // payload 필드 추출
        String payload = wrapperDto.payload();

        // payload를 CreateDeliveryRequest로 다시 파싱
        // todo CreateDeliveryRequest 대신 WrapperRequest로 다시 작성 필요
        CreateDeliveryRequest request = JsonHelper.fromJson(payload, CreateDeliveryRequest.class);
        log.info("📥 Kafka 배송 생성 메시지 수신: {}", request);

        try {
            deliveryService.createDelivery(request);
            log.info("👉 배송 생성 성공! 배송 생성이 정상적으로 처리되었습니다.");
        } catch (NotFoundException e) {
            log.warn("⚠️ 필수 데이터 누락으로 메시지 처리 실패 ({}): {}", e.getErrorCode(), e.getMessage());
        }

    }
}
