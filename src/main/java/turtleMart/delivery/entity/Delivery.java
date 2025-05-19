package turtleMart.delivery.entity;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import turtleMart.member.entity.Address;
import turtleMart.member.entity.Seller;
import turtleMart.order.entity.Order;

import java.time.LocalDateTime;

@Entity
@Getter
@Table
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class Delivery {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne
    @JoinColumn
    private Order order;

    @ManyToOne
    @JoinColumn
    private Seller seller;

    @ManyToOne
    @JoinColumn
    private Address address;

    private DeliveryStatus deliveryStatus;

    private String address2;

    private String addressDetail;

    private LocalDateTime shippedAt;

    private LocalDateTime deliveredAt;

}
