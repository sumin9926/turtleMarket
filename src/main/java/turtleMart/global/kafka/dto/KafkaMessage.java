package turtleMart.global.kafka.dto;

public record KafkaMessage<T>(
    String type,
    T payload
) {
}
