package turtleMart.review.dto.request;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;

public record CreateReviewTemplateRequest(@NotBlank @Size(max = 50) String question,
                                          @NotBlank @Size(max = 30) String bad,
                                          @NotBlank @Size(max = 30) String normal,
                                          @NotBlank @Size(max = 30) String good) {
}
