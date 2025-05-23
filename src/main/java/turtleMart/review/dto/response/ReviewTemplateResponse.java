package turtleMart.review.dto.response;

import turtleMart.review.entity.ReviewTemplate;

public record ReviewTemplateResponse(Long id, String question, String satisfaction_low, String satisfaction_medium, String satisfaction_high) {

    public static ReviewTemplateResponse from(ReviewTemplate reviewTemplate) {
        return new ReviewTemplateResponse(
                reviewTemplate.getId(),
                reviewTemplate.getQuestion(),
                reviewTemplate.getSatisfaction_low(),
                reviewTemplate.getSatisfaction_medium(),
                reviewTemplate.getSatisfaction_high()
        );
    }
}
