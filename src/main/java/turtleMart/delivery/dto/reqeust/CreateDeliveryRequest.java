package turtleMart.delivery.dto.reqeust;

public record CreateDeliveryRequest(
    Long orderId,
    Long sellerId,
    Long senderId,
    Long addressId,
    String deliveryRequest
) {
}
