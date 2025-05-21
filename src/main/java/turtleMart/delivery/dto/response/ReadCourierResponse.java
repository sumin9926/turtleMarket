package turtleMart.delivery.dto.response;

import turtleMart.delivery.entity.Courier;

import java.time.LocalDateTime;

public record ReadCourierResponse(
    Long id,
    String name,
    String code,
    String trackingUrlTemplate,
    LocalDateTime createdAt
) {
    public static ReadCourierResponse from(Courier courier) {
        return new ReadCourierResponse(
            courier.getId(),
            courier.getName(),
            courier.getCode(),
            courier.getTrackingUrlTemplate(),
            courier.getCreatedAt()
        );
    }
}
