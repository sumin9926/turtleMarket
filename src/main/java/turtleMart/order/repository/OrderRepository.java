package turtleMart.order.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import turtleMart.order.entity.Order;

import java.util.List;
import java.util.Optional;

public interface OrderRepository extends JpaRepository<Order, Long> {

    @Query("SELECT o FROM Order o JOIN FETCH o.orderItems WHERE o.id=:orderId")
    Optional<Order> findWithOrderItemsById(@Param("orderId") Long orderId);

    @Query("SELECT o FROM Order o JOIN FETCH o.orderItems WHERE o.member.id=:memberId")
    List<Order> findWithOrderItemsByMemberId(@Param("memberId") Long memberId);
}
