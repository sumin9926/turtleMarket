package turtleMart.delivery.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import turtleMart.delivery.entity.Sender;

import java.util.List;

public interface SenderRepository extends JpaRepository<Sender, Long> {

    boolean existsByName(String name);

    @Query("SELECT s FROM Sender s JOIN FETCH s.courier WHERE s.isDeleted = false")
    List<Sender> findAllByIsDeletedFalse();
}
