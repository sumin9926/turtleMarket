package turtleMart.member.entity;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import turtleMart.member.dto.request.SellerRegisterRequest;

@Entity
@Getter
@Table
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class Seller {

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
        this.authority = Authority.SELLER;
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
}
