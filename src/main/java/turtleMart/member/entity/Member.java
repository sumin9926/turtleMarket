package turtleMart.member.entity;


import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import turtleMart.global.common.BaseEntity;
import turtleMart.member.dto.request.SignupRequest;
import turtleMart.member.dto.request.updateProfileRequest;

@Entity
@Getter
@Table
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class Member extends BaseEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    @Enumerated(EnumType.STRING)
    private Membership memberShip;

    @Enumerated(EnumType.STRING)
    private Authority authority;

    private String name;

    private String email;

    private String password;

    private String phoneNumber;

    public Member(Membership memberShip, Authority authority, String name, String email, String password, String phoneNumber) {
        this.memberShip = memberShip;
        this.authority = authority;
        this.name = name;
        this.email = email;
        this.password = password;
        this.phoneNumber = phoneNumber;
    }

    public static Member of(SignupRequest request, String password) {
        return new Member(
                Membership.STANDARD,
                Authority.CUSTOMER,
                request.name(),
                request.email(),
                password,
                request.phoneNumber()
        );
    }

    public void updateProfile(updateProfileRequest request) {
        this.name = request.name() != null ? request.name() : this.name;
        this.email = request.email() != null ? request.email() : this.email;
        this.phoneNumber = request.phoneNumber() != null ? request.phoneNumber() : this.phoneNumber;
    }

    public void updatePassword(String password) {
        this.password = password;
    }

    public void registerSeller() {
        this.authority = Authority.SELLER;
    }

    public void unregisterSeller() {
        this.authority = Authority.CUSTOMER;
    }
}
