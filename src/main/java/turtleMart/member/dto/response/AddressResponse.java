package turtleMart.member.dto.response;

import turtleMart.member.entity.Address;

public record AddressResponse(
        Long addressId,
        String name,
        String address,
        String detailAddress
) {
    public static AddressResponse from(Address address) {
        return new AddressResponse(
                address.getId(),
                address.getName(),
                address.getAddress(),
                address.getDetailAddress()
        );
    }
}
