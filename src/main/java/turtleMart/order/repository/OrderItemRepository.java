package turtleMart.order.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import turtleMart.order.entity.OrderItem;

public interface OrderItemRepository extends JpaRepository<OrderItem, Long> {
}
