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

    private Integer totalAmount;

    private Integer deductedAmount;

    private Integer actualAmount;

    @Enumerated(EnumType.STRING)
    private PaymentMethod paymentMethod;

    private String cardCompany;

    private Integer installmentMonth;

    public Payment(
            Order order, Member member, Integer amount, PaymentMethod paymentMethod,
            String cardCompany, Integer installmentMonth) {
        this.order = order;
        this.member = member;
        this.totalAmount = amount;
        this.deductedAmount = 0;
        this.actualAmount = amount;
        this.paymentMethod = paymentMethod;
        this.cardCompany = cardCompany;
        this.installmentMonth = installmentMonth == null ? 1 : installmentMonth;
    }

    public static Payment of(
            Order order, Member member, Integer amount, PaymentMethod paymentMethod,
            String cardCompany, Integer installmentMonth) {
        return new Payment(order, member, amount, paymentMethod, cardCompany, installmentMonth);
    }

    public void deduct(Integer amount) {
        this.deductedAmount += amount;
        this.actualAmount -= amount;
    }
}
