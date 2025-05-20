package turtleMart.review.dto.response;

import turtleMart.review.entity.ReviewTemplate;

public record ReviewTemplateResponse(Long id, String question, String bad, String normal, String good) {

    public static ReviewTemplateResponse from(ReviewTemplate reviewTemplate) {
        return new ReviewTemplateResponse(
                reviewTemplate.getId(),
                reviewTemplate.getQuestion(),
                reviewTemplate.getBad(),
                reviewTemplate.getNormal(),
                reviewTemplate.getGood()
        );
    }
}
