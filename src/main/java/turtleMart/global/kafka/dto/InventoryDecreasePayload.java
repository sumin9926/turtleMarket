package turtleMart.global.kafka.dto;

public record InventoryDecreasePayload(
    Long orderId,
    Long sellerId,
    Long senderId,
    Long addressId,
    String deliveryRequest
) {
}
