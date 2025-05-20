package turtleMart.review.dto.response;

import turtleMart.review.entity.ReviewTemplate;

public record ReviewTemplateResponse(Long id, String question, String low, String medium, String high) {

    public static ReviewTemplateResponse from(ReviewTemplate reviewTemplate) {
        return new ReviewTemplateResponse(
                reviewTemplate.getId(),
                reviewTemplate.getQuestion(),
                reviewTemplate.getLow(),
                reviewTemplate.getMedium(),
                reviewTemplate.getHigh()
        );
    }
}
