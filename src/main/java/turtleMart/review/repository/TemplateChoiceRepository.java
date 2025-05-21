package turtleMart.review.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import turtleMart.review.entity.TemplateChoice;

import java.util.List;

public interface TemplateChoiceRepository extends JpaRepository<TemplateChoice, Long> {

    @Query("""
            SELECT t FROM TemplateChoice t JOIN FETCH t.productReviewTemplate p JOIN FETCH p.reviewTemplate
            """)
    List<TemplateChoice> findByReviewId(@Param("reviewId") Long reviewId);
}
