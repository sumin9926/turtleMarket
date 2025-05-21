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

    @ManyToOne
    @JoinColumn
    private Order order;

    @ManyToOne
    @JoinColumn
    private Member member;

    private int amount;

    private PaymentMethod paymentMethod;

    private String cardCompany;

    private Integer installmentMonth;

    private PaymentStatus paymentStatus;

    private FailReason failReason;
}
