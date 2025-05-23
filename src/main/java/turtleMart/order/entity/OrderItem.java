package turtleMart.order.entity;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import turtleMart.product.entity.Product;

@Entity
@Getter
@Table
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class OrderItem {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Setter
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "order_id")
    private Order order;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "product_id")
    private Product product;

    private Integer price;

    private String name;

    private Integer quantity;

    private OrderItemStatus orderItemStatus;

    private OrderItem(Order order, Product product, Integer price, String name, Integer quantity, OrderItemStatus orderItemStatus) {
        this.order = order;
        this.product = product;
        this.price = price;
        this.name = name;
        this.quantity = quantity;
        this.orderItemStatus = orderItemStatus;
    }

    public static OrderItem of(Order order, Product product, Integer price, String name, Integer quantity) {
        return new OrderItem(order, product, price, name, quantity, OrderItemStatus.UNPAID);
    }

    public void updateStatus(OrderItemStatus status) {
        this.orderItemStatus = status;
    }
}
