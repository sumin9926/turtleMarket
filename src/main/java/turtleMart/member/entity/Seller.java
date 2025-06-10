package turtleMart.member.entity;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import turtleMart.global.common.BaseEntity;
import turtleMart.member.dto.request.SellerRegisterRequest;
import turtleMart.member.dto.request.UpdateSellerRequest;

@Entity
@Getter
@Table
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class Seller extends BaseEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @OneToOne
    @JoinColumn
    private Member member;

    private Authority authority;

    private String businessName;

    private String businessAddress;

    private String account;

    private String businessLicense;

    public Seller(Member member, String businessName, String businessAddress, String account, String businessLicense) {
        this.member = member;
        this.authority = member.getAuthority();
        this.businessName = businessName;
        this.businessAddress = businessAddress;
        this.account = account;
        this.businessLicense = businessLicense;
    }

    public static Seller of(SellerRegisterRequest request, Member member) {
        return new Seller(
                member,
                request.businessName(),
                request.businessAddress(),
                request.account(),
                request.businessLicense()
        );
    }

    public void updateSeller(UpdateSellerRequest request) {
        this.businessName = request.businessName() != null ? request.businessName() : this.businessName;
        this.businessAddress = request.businessAddress() != null ? request.businessAddress() : this.businessAddress;
        this.account = request.businessAddress() != null ? request.account() : this.account;
        this.businessLicense = request.businessLicense() != null ? request.businessLicense() : this.businessLicense;
    }
}
