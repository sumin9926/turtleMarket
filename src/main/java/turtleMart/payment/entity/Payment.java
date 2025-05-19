package turtleMart.payment.entity;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import turtleMart.member.entity.Member;
import turtleMart.order.entity.Order;

@Entity
@Getter
@Table
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class Payment {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne
    @JoinColumn
    private Order order;

    @ManyToOne
    @JoinColumn
    private Member member;

    private String amount;

    private String paymentMethod;

    private String cardCompany;

    private Integer installmentMonth;

    private PaymentStatus paymentStatus;

    private FailReason failReason;
}
