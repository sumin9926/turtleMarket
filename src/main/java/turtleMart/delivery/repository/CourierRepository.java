package turtleMart.delivery.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import turtleMart.delivery.entity.Courier;

import java.util.List;

public interface CourierRepository extends JpaRepository<Courier, Long> {

    boolean existsByNameAndCode(String name, String code);

    @Query("SELECT c FROM Courier c WHERE c.isDeleted = false")
    List<Courier> findAllByIsDeletedFalse();
}
