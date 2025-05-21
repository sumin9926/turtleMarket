package turtleMart.payment.entity;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import turtleMart.global.common.BaseEntity;
import turtleMart.member.entity.Member;
import turtleMart.order.entity.Order;

@Entity
@Getter
@Table
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class Payment extends BaseEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "order_id", nullable = false)
    private Order order;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "member_id", nullable = false)
    private Member member;

    private int amount;

    private PaymentMethod paymentMethod;

    private String cardCompany;

    private Integer installmentMonth;

    private PaymentStatus paymentStatus;

    private FailReason failReason;

    public Payment(
            Order order, Member member, int amount, PaymentMethod paymentMethod,
            String cardCompany, Integer installmentMonth) {
        this.order = order;
        this.member = member;
        this.amount = amount;
        this.paymentMethod = paymentMethod;
        this.cardCompany = cardCompany;
        this.installmentMonth = installmentMonth;
        this.paymentStatus = PaymentStatus.PENDING;
    }

    public static Payment of(
            Order order, Member member, int amount, PaymentMethod paymentMethod,
            String cardCompany, Integer installmentMonth) {
        return new Payment(order, member, amount, paymentMethod, cardCompany, installmentMonth);
    }
}
