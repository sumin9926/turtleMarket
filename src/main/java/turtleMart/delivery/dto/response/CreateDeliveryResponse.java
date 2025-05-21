package turtleMart.delivery.dto.response;

import turtleMart.delivery.entity.Delivery;
import turtleMart.delivery.entity.DeliveryStatus;

import java.time.LocalDateTime;

public record CreateDeliveryResponse(
    Long id,
    DeliveryStatus deliveryStatus,
    String deliveryRequest,
    String receiverName,
    String receiverPhone,
    String receiverAddress,
    String receiverDetailAddress,
    LocalDateTime createdAt
) {
    public static CreateDeliveryResponse from(Delivery delivery) {
        return new CreateDeliveryResponse(
            delivery.getId(),
            delivery.getDeliveryStatus(),
            delivery.getDeliveryRequest(),
            delivery.getReceiverName(),
            delivery.getReceiverPhone(),
            delivery.getReceiverAddress(),
            delivery.getReceiverDetailAddress(),
            delivery.getCreatedAt()
        );
    }
}
