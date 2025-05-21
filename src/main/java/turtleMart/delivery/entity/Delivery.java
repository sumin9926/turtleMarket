package turtleMart.delivery.entity;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import turtleMart.global.common.BaseEntity;
import turtleMart.member.entity.Address;
import turtleMart.member.entity.Seller;
import turtleMart.order.entity.Order;

import java.time.LocalDateTime;

@Entity
@Getter
@Table
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class Delivery extends BaseEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "order_id", nullable = false)
    private Order order;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "seller_id", nullable = false)
    private Seller seller;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "sender_id", nullable = false)
    private Sender sender;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "address_id", nullable = false)
    private Address address;

    @Enumerated(EnumType.STRING)
    @Column(name = "delivery_status", nullable = false)
    private DeliveryStatus deliveryStatus = DeliveryStatus.PREPARING;

    @Column(nullable = false)
    private String deliveryRequest;

    @Column(unique = true)
    private String trackingNumber;

    @Column(nullable = false)
    private String receiverName;

    @Column(nullable = false)
    private String receiverPhone;

    @Column(nullable = false)
    private String receiverAddress;

    @Column(nullable = false)
    private String receiverDetailAddress;

    private LocalDateTime shippedAt;

    private LocalDateTime deliveredAt;

    private Delivery(Order order, Seller seller, Sender sender, Address address, String deliveryRequest) {
        this.order = order;
        this.seller = seller;
        this.sender = sender;
        this.address = address;
        this.deliveryRequest = deliveryRequest;
        this.receiverName = address.getReceiverName();
        this.receiverPhone = address.getReceiverPhone();
        this.receiverAddress = address.getAddress();
        this.receiverDetailAddress = address.getDetailAddress();
    }

    public static Delivery of(Order order, Seller seller, Sender sender, Address address, String deliveryRequest) {
        return new Delivery(order, seller, sender, address, deliveryRequest);
    }
}
