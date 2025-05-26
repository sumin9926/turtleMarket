package turtleMart.review.dto.request;

import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;

public record CancelReviewReportRequest(@NotBlank @Size(min = 15) String cancelReason) {
}
