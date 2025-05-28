package turtleMart.member.dto.request;

public record UpdateAddressRequest(
        String name,
        String address,
        String detailAddress
) {
}
