package turtleMart.review.dto.request;

import jakarta.validation.constraints.NotBlank;

public record UpdateReasonCodeRequest(@NotBlank String reason) {
}
