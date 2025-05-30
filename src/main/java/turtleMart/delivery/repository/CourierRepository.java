package turtleMart.delivery.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import turtleMart.delivery.entity.Courier;

import java.util.List;

public interface CourierRepository extends JpaRepository<Courier, Long> {

    boolean existsByNameAndCode(String name, String code);

    List<Courier> findAllByIsDeletedFalse();

    boolean existsByIdAndIsDeletedFalse(Long courierId);
}
