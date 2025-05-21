package turtleMart.delivery.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import turtleMart.delivery.entity.Delivery;

public interface DeliveryRepository extends JpaRepository<Delivery, Long> {
}
