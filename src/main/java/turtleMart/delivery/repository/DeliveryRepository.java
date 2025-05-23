package turtleMart.delivery.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import turtleMart.delivery.entity.Delivery;

import java.util.List;

public interface DeliveryRepository extends JpaRepository<Delivery, Long> {

    @Query("SELECT d FROM Delivery d WHERE d.address.member.id = :memberId")
    List<Delivery> findAllByMemberId(Long memberId);

    @Query("SELECT d FROM Delivery d WHERE d.seller.id = :sellerId")
    List<Delivery> findAllBySeller(Long sellerId);

    @Query("SELECT d FROM Delivery d JOIN FETCH d.order WHERE d.order.id=:orderId")
    List<Delivery> findAllWithOrderIds(@Param("orderIds") List<Long> orderIdList);
}
