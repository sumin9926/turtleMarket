package turtleMart.delivery.dto.reqeust;

public record CreateSenderRequest(
    Long courierId,
    String name,
    String phoneNumber,
    String address,
    String detailAddress
) {
}
