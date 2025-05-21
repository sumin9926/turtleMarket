package turtleMart.review.dto.request;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;

public record UpdateTemplateChoiceRequest(@NotNull Long templateChoiceId,  @NotBlank String answer) {
}
