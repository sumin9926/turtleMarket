package turtleMart.review.dto.request;

import jakarta.validation.constraints.NotBlank;

public record CreateReasonCodeRequest(@NotBlank String reason) {
}
