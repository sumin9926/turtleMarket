package turtleMart.payment.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import turtleMart.payment.entity.Payment;

import java.util.List;
import java.util.Optional;

public interface PaymentRepository extends JpaRepository<Payment, Long> {
    Optional<List<Payment>> findAllByMemberId(Long memberId);
}
