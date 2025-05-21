package turtleMart.review.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import turtleMart.review.entity.ReasonCode;

import java.util.List;

public interface ReasonCodeRepository extends JpaRepository<ReasonCode, Long> {

    @Query("SELECT r FROM ReasonCode r WHERE r.isDeleted = FALSE ")
    List<ReasonCode> findAllIsDeletedFalse();
}
