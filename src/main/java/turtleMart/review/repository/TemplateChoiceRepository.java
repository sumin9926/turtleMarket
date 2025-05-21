package turtleMart.review.repository;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import turtleMart.review.entity.Review;
import turtleMart.review.entity.TemplateChoice;

import java.util.List;

public interface TemplateChoiceRepository extends JpaRepository<TemplateChoice, Long> {

    @Query("""
            SELECT t FROM TemplateChoice t JOIN FETCH t.productReviewTemplate p JOIN FETCH p.reviewTemplate WHERE t.review.id = :reviewId
            """)
    List<TemplateChoice> findByReviewId(@Param("reviewId") Long reviewId);


    @Query("""
            SELECT t FROM TemplateChoice t JOIN FETCH t.productReviewTemplate p JOIN FETCH p.reviewTemplate WHERE t.review.member.id = :memberId
            """)
    Page<TemplateChoice> findByMemberId(Long memberId, Pageable pageable);

    @Modifying
    @Query("DELETE TemplateChoice t WHERE t.review.id = :reviewId")
    void deleteAllByReviewId(@Param("reviewId") Long reviewId);
}
