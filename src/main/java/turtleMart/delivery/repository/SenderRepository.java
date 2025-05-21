package turtleMart.delivery.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import turtleMart.delivery.entity.Sender;

public interface SenderRepository extends JpaRepository<Sender, Long> {

    boolean existsByName(String name);
}
