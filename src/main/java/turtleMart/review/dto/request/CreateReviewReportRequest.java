package turtleMart.review.dto.request;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;

public record CreateReviewReportRequest(@NotNull Long reasonCodeId, @NotBlank(message = "신고 이유는 필수 값입니다") String ReasonDetail) {
}
