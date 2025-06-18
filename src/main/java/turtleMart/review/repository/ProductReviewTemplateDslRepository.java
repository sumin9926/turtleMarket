package turtleMart.review.repository;


import turtleMart.review.entity.ProductReviewTemplate;
import java.util.List;


public interface ProductReviewTemplateDslRepository {

    List<ProductReviewTemplate> findByIdInWithReviewTemplate(List<Long> productReviewTemplateIdList);

    boolean notExistsByProductIdAndReviewTemplateId(Long productId, List<Long> reviewTemplateIdList);
}
