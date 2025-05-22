package turtleMart.review.dto.request;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;

public record CreateReviewReportRequest(@NotNull Long reasonCodeId, @NotBlank String ReasonDetail) {
}
