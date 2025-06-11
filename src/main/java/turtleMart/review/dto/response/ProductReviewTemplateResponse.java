package turtleMart.review.dto.response;

import turtleMart.review.entity.ProductReviewTemplate;

public record ProductReviewTemplateResponse(Long id, ReviewTemplateResponse templateResponse) {

    public static ProductReviewTemplateResponse from(ProductReviewTemplate productReviewTemplate){

        ReviewTemplateResponse templateResponse = ReviewTemplateResponse.from(productReviewTemplate.getReviewTemplate());
        return new ProductReviewTemplateResponse(productReviewTemplate.getId(), templateResponse);
    }
}
