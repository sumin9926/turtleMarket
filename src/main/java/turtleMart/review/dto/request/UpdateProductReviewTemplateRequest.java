package turtleMart.review.dto.request;

import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;

import java.util.List;

public record UpdateProductReviewTemplateRequest(@NotNull Long sellerId,
                                                 @NotNull @Size(max = 3, message = "리뷰 템플릿은 최대 3개까지 선택 가능합니다.") List<Long> reviewTemplateIdList) {
}
