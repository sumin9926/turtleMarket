package turtleMart.global.kafka.dto;

import turtleMart.global.kafka.enums.OperationType;

public record OperationWrapperDto(
        OperationType operationType,
        String payload
) {
    public static OperationWrapperDto from(OperationType operationType, String payload){
        return new OperationWrapperDto(operationType, payload);
    }
}
