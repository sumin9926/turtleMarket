package turtleMart.review.dto.request;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;

public record CreateTemplateChoiceRequest(@NotNull Long productReviewTemplateId, @NotBlank String answer) {
}
