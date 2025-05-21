package turtleMart.payment.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import turtleMart.payment.entity.Payment;

public interface PaymentRepository extends JpaRepository<Payment, Integer> {
}
