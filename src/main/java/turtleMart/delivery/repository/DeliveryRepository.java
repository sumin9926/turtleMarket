package turtleMart.delivery.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import turtleMart.delivery.entity.Delivery;

import java.util.List;

public interface DeliveryRepository extends JpaRepository<Delivery, Long>, DeliveryQueryRepository {

    @Query("SELECT d FROM Delivery d JOIN FETCH d.order WHERE d.order.id=:orderId")
    List<Delivery> findAllWithOrderIds(@Param("orderIds") List<Long> orderIdList);
}
