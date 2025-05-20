package turtleMart.review.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import turtleMart.review.entity.ReviewTemplate;

public interface ReviewTemplateRepository extends JpaRepository<ReviewTemplate, Long> {
}
