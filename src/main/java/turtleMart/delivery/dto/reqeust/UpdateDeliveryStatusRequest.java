package turtleMart.delivery.dto.reqeust;

import turtleMart.delivery.entity.DeliveryStatus;

public record UpdateDeliveryStatusRequest(
    DeliveryStatus deliveryStatus
) {
}
