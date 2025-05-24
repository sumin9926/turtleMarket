package turtleMart.member.entity;


import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import turtleMart.member.dto.request.SignupRequest;

@Entity
@Getter
@Table
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class Member {

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
}
