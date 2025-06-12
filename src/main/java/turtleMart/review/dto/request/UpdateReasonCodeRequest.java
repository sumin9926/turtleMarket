package turtleMart.review.dto.request;

import jakarta.validation.constraints.NotBlank;

public record UpdateReasonCodeRequest(@NotBlank(message = "신고 이유는 필수값입니다") String reason) {
}
