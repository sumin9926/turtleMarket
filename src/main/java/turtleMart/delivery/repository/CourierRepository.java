package turtleMart.delivery.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import turtleMart.delivery.entity.Courier;

public interface CourierRepository extends JpaRepository<Courier, Long> {

    boolean existsByNameAndCode(String name, String code);
}
