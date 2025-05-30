package turtleMart.order.entity;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import turtleMart.product.entity.ProductOptionCombination;

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
    @JoinColumn(name = "product_option_combination_id")
    private ProductOptionCombination productOptionCombination;

    private Integer price;

    private String name;

    private Integer quantity;

    @Enumerated(EnumType.STRING)
    private OrderItemStatus orderItemStatus;

    private OrderItem(Order order, ProductOptionCombination productOptionCombination, Integer price, String name, Integer quantity, OrderItemStatus orderItemStatus) {
        this.order = order;
        this.productOptionCombination = productOptionCombination;
        this.price = price;
        this.name = name;
        this.quantity = quantity;
        this.orderItemStatus = orderItemStatus;
    }

    public static OrderItem of(Order order, ProductOptionCombination productOptionCombination, Integer price, String name, Integer quantity) {
        return new OrderItem(order, productOptionCombination, price, name, quantity, OrderItemStatus.UNPAID);
    }

    public void updateStatus(OrderItemStatus status) {
        this.orderItemStatus = status;
    }
}
