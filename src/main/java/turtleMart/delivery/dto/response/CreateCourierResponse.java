package turtleMart.delivery.dto.response;

import turtleMart.delivery.entity.Courier;

import java.time.LocalDateTime;

public record CreateCourierResponse(
    Long id,
    String name,
    String code,
    String trackingUrlTemplate,
    LocalDateTime createdAt
) {
    public static CreateCourierResponse from(Courier courier) {
        return new CreateCourierResponse(
            courier.getId(),
            courier.getName(),
            courier.getCode(),
            courier.getTrackingUrlTemplate(),
            courier.getCreatedAt()
        );
    }
}
