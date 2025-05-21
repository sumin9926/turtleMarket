package turtleMart.review.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import turtleMart.review.entity.ReviewTemplate;

import java.util.List;
import java.util.Optional;

public interface ReviewTemplateRepository extends JpaRepository<ReviewTemplate, Long> {

    @Query("SELECT r FROM ReviewTemplate r WHERE r.isDeleted = FALSE ")
    List<ReviewTemplate> findAllDeletedFalse();


}
