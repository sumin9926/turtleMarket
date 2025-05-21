package turtleMart.delivery.dto.response;

import turtleMart.delivery.entity.Delivery;
import turtleMart.delivery.entity.DeliveryStatus;

public record ReadDeliveryResponse(
    Long id,
    String receiverName,
    String receiverAddress,
    DeliveryStatus deliveryStatus,
    String trackingNumber,
    String courierName,
    String trackingUrl
) {
    public static ReadDeliveryResponse from(Delivery delivery) {
        return new ReadDeliveryResponse(
            delivery.getId(),
            delivery.getReceiverName(),
            delivery.getReceiverAddress(),
            delivery.getDeliveryStatus(),
            delivery.getTrackingNumber(),
            delivery.getSender().getCourier().getName(),
            delivery.getSender().getCourier().getTrackingUrlTemplate().replace("{trackingNumber}", delivery.getTrackingNumber())
        );
    }
}
