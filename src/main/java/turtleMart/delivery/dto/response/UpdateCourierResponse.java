package turtleMart.delivery.dto.response;

import turtleMart.delivery.entity.Courier;

import java.time.LocalDateTime;

public record UpdateCourierResponse(
    Long id,
    String name,
    String code,
    String trackingUrlTemplate,
    LocalDateTime updatedAt
) {
    public static UpdateCourierResponse from(Courier courier) {
        return new UpdateCourierResponse(
            courier.getId(),
            courier.getName(),
            courier.getCode(),
            courier.getTrackingUrlTemplate(),
            courier.getUpdatedAt()
        );
    }
}
