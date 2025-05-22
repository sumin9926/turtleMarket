package turtleMart.delivery.dto.response;

import turtleMart.delivery.entity.Delivery;
import turtleMart.delivery.entity.DeliveryStatus;

import java.time.LocalDateTime;

public record UpdateDeliveryResponse(
    Long id,
    DeliveryStatus deliveryStatus,
    String deliveryRequest,
    String trackingNumber,
    String receiverName,
    String receiverPhone,
    String receiverAddress,
    String receiverDetailAddress,
    LocalDateTime shippedAt,
    LocalDateTime deliveredAt
) {
    public static UpdateDeliveryResponse from(Delivery delivery) {
        return new UpdateDeliveryResponse(
            delivery.getId(),
            delivery.getDeliveryStatus(),
            delivery.getDeliveryRequest(),
            delivery.getTrackingNumber(),
            delivery.getReceiverName(),
            delivery.getReceiverPhone(),
            delivery.getReceiverAddress(),
            delivery.getReceiverDetailAddress(),
            delivery.getShippedAt(),
            delivery.getDeliveredAt()
        );
    }
}
