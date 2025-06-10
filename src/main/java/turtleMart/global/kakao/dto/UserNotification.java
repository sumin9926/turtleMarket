package turtleMart.global.kakao.dto;

import turtleMart.delivery.entity.Delivery;

public record UserNotification(
    Long orderId,
    String trackingNumber,
    String senderName,
    String courierName,
    String trackingUrlTemplate

) {
    public static UserNotification from(Delivery delivery) {
        return new UserNotification(
            delivery.getOrder().getId(),
            delivery.getTrackingNumber(),
            delivery.getSender().getName(),
            delivery.getSender().getCourier().getName(),
            delivery.getSender().getCourier().getTrackingUrlTemplate()
        );
    }
}
