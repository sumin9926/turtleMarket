package turtleMart.member.entity;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import turtleMart.member.dto.request.CardRegisterRequest;

@Entity
@Getter
@Table
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class Card {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne
    @JoinColumn
    private Member member;

    private String cardNumber;

    private String expirationDate;

    private String cvcNumber;

    private String cardPassword;

    public Card(String cardNumber, String expirationDate, String cvcNumber, String cardPassword) {
        this.cardNumber = cardNumber;
        this.expirationDate = expirationDate;
        this.cvcNumber = cvcNumber;
        this.cardPassword = cardPassword;
    }

    public static Card of(CardRegisterRequest request) {
        return new Card(
                request.cardNumber(),
                request.expirationDate(),
                request.cvcNumber(),
                request.cardPassword()
        );
    }
}
