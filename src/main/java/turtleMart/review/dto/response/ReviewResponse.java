package turtleMart.review.dto.response;

import turtleMart.review.entity.Review;

import java.time.LocalDateTime;
import java.util.List;

public record ReviewResponse(Long id,
                             Long memberId,
                             Long productId,
                             Long orderItemId,
                             String title,
                             String content,
                             Integer rating,
                             List<String> imageUrlList,
                             List<TemplateChoiceResponse>  templateChoiceResponseList,
                             LocalDateTime createdAt,
                             LocalDateTime updatedAt
) {
    public static ReviewResponse of(Review review, List<String> imageUrlList, List<TemplateChoiceResponse> templateChoiceResponseList){
        return new ReviewResponse(
                review.getId(),
                review.getMember().getId(),
                review.getProduct().getId(),
                review.getOrderItem().getId(),
                review.getTitle(),
                review.getContent(),
                review.getRating(),
                imageUrlList,
                templateChoiceResponseList,
                review.getCreatedAt(),
                review.getUpdatedAt()
        );
    }
}
