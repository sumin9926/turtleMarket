package turtleMart.review.dto.request;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;

public record UpdateReviewTemplateRequest(@NotBlank @Size(max = 50) String question,
                                          @NotBlank @Size(max = 30) String satisfaction_low,
                                          @NotBlank @Size(max = 30) String satisfaction_medium,
                                          @NotBlank @Size(max = 30) String high) {
}
