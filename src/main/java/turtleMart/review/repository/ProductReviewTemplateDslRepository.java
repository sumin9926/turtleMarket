package turtleMart.review.repository;


import turtleMart.review.entity.ProductReviewTemplate;

import java.util.List;
import java.util.Optional;


public interface ProductReviewTemplateDslRepository {

    List<ProductReviewTemplate> findByIdInWithReviewTemplate(List<Long> productReviewTemplateIdList);

    Optional<ProductReviewTemplate> findByIdWithReviewTemplate(Long productReviewTemplateId);

    boolean existsByProductIdAndReviewTemplateId(Long productId, List<Long> reviewTemplateIdList);
}
