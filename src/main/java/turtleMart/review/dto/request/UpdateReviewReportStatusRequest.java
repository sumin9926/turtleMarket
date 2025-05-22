package turtleMart.review.dto.request;

import jakarta.validation.constraints.NotBlank;

public record UpdateReviewReportStatusRequest(@NotBlank String reviewReportStatus) {
}
