package turtleMart.delivery.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import turtleMart.delivery.entity.Delivery;

import java.util.List;

public interface DeliveryRepository extends JpaRepository<Delivery, Long> {

    @Query("SELECT d FROM Delivery d WHERE d.address.member.id = :memberId")
    List<Delivery> findAllByMemberId(Long memberId);
}
