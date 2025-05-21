package turtleMart.delivery.dto.reqeust;

public record UpdateSenderRequest(
    String name,
    String phoneNumber,
    String address,
    String detailAddress
) {
}
