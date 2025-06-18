package turtleMart.delivery.dto.reqeust;

public record CreateDeliveryRequest(
    Long orderId,
    Long sellerId,
    Long senderId,
    Long addressId,
    String deliveryRequest
) {
    public static CreateDeliveryRequest updateOrderId(CreateDeliveryRequest deliveryRequest, Long orderId){
        return new CreateDeliveryRequest(
                orderId,
                deliveryRequest.sellerId(),
                deliveryRequest.senderId(),
                deliveryRequest.addressId(),
                deliveryRequest.deliveryRequest()
        );
    }
}
