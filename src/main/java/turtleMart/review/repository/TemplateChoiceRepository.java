package turtleMart.review.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import turtleMart.review.entity.TemplateChoice;

public interface TemplateChoiceRepository extends JpaRepository<TemplateChoice, Long> {
}
