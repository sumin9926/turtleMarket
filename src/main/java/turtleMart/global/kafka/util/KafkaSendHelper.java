package turtleMart.global.kafka.util;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class KafkaSendHelper {

    private final KafkaTemplate<String, String> stringKafkaTemplate; //기본 템플릿

    public <K, V> void sendWithCallback(KafkaTemplate<K, V> template, String topic, K key, V value) {
        template.send(topic, key, value)
                .whenComplete((result, ex) -> {
                    if (ex != null) {
                        log.error("Kafka 전송 실패 - topic={}, key={}", topic, key, ex);
                        // TODO kafka 전송 실패 시 롤백 방법
                    } else {
                        log.info("Kafka 전송 성공 - topic={}, key={}, offset={}",
                                topic, key, result.getRecordMetadata().offset());
                    }
                });
    }

    // 기본 Kafka 전송 유틸
    public void send(String topic, String key, String value) {
        sendWithCallback(stringKafkaTemplate, topic, key, value);
    }
}
