package turtleMart.payment.repository;

import io.lettuce.core.dynamic.annotation.Param;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import turtleMart.payment.entity.Payment;

import java.util.List;
import java.util.Optional;

public interface PaymentRepository extends JpaRepository<Payment, Long> {
    Optional<List<Payment>> findAllByMemberId(Long memberId);

    @Query("""
SELECT p
FROM Payment p JOIN FETCH p.order o JOIN FETCH o.member m
WHERE p.order.id=:orderId
""")
    Optional<Payment> findByOrderId(@Param("orderId") Long orderId);
}
