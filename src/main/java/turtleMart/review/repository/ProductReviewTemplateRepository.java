package turtleMart.review.repository;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import turtleMart.review.entity.ProductReviewTemplate;
import turtleMart.review.entity.Review;

import java.util.List;

public interface ProductReviewTemplateRepository extends JpaRepository<ProductReviewTemplate, Long> {

    @Query("""
                SELECT p
                FROM ProductReviewTemplate p
                JOIN FETCH p.reviewTemplate
                WHERE p.product.id = :productId AND p.isDeleted = FALSE
            """)
    List<ProductReviewTemplate> findByProductId(@Param("productId") Long productId);


    @Query(""" 
            SELECT p
            FROM ProductReviewTemplate p
            WHERE p.reviewTemplate.id = :reviewTemplateId AND p.isDeleted = FALSE
            """)
    List<ProductReviewTemplate> findByReviewTemplateId(@Param(("reviewTemplateId")) Long reviewTemplateId);

    @Query("SELECT r FROM ProductReviewTemplate r WHERE r.id in(:idList) AND r.reviewTemplate.isDeleted = FALSE ")
    List<ProductReviewTemplate> findAllByIdDeletedFalse(@Param("idList") List<Long> idList);




}
