package turtleMart.member.entity;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import turtleMart.global.common.BaseEntity;
import turtleMart.member.dto.request.AddressRegisterRequest;
import turtleMart.member.dto.request.UpdateAddressRequest;

@Entity
@Getter
@Table
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class Address extends BaseEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn
    private Member member;

    private String name;

    private String address;

    private String detailAddress;

    private String receiverName;

    private String receiverPhone;

    public Address(String name, String address, String detailAddress) {
        this.name = name;
        this.address = address;
        this.detailAddress = detailAddress;
    }

    public static Address of(AddressRegisterRequest request) {
        return new Address(
                request.name(),
                request.address(),
                request.detailAddress()
        );
    }

    public void updateAddress(UpdateAddressRequest request) {
        this.name = request.name() != null ? request.name() : this.name;
        this.address = request.address() != null ? request.address() : this.address;
        this.detailAddress = request.detailAddress() != null ? request.detailAddress() : this.detailAddress;
    }
}
