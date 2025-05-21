package turtleMart.order.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import turtleMart.order.entity.Order;

public interface OrderRepository extends JpaRepository<Order, Long> {
}
