package turtleMart.review.dto.request;

import jakarta.validation.constraints.NotNull;

import java.util.List;

public record CreateProductReviewTemplateRequest(@NotNull Long sellerId, @NotNull List<Long> reviewTemplateIdList) {
}
