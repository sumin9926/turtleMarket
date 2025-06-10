package turtleMart.global.kafka.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;
import turtleMart.global.kafka.dto.KafkaMessage;
import turtleMart.global.kafka.dto.InventoryDecreasePayload;
import turtleMart.global.kafka.producer.ProductProducer;

@RestController
@RequiredArgsConstructor
public class TestController {

    private final ProductProducer productProducer;

    @PostMapping("/send")
    public ResponseEntity<String> sendTestMessage(@RequestBody KafkaMessage<InventoryDecreasePayload> message) {
        productProducer.sendStockDecreaseMessage(message);

        return ResponseEntity.status(HttpStatus.OK).body("\uD83D\uDCE4 Kafka 메시지 전송");
    }
}
