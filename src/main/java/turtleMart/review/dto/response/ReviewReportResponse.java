package turtleMart.review.dto.response;

import turtleMart.review.entity.Review;
import turtleMart.review.entity.ReviewReport;

import java.time.LocalDateTime;
import java.util.List;

public record ReviewReportResponse(
        Long id,
        Long memberId,
        String reasonCode,
        Boolean isProcessed,
        String ReasonDetail,
        LocalDateTime createdAt,
        LocalDateTime updatedAt,
        ReviewResponse reviewResponse
) {
    public static ReviewReportResponse of(Review review, List<String> imageUrlList,
                                        List<TemplateChoiceResponse> templateChoiceResponseList, ReviewReport reviewReport){

        return new ReviewReportResponse(
                reviewReport.getId(),
                reviewReport.getMember().getId(),
                reviewReport.getReasonCode().getReason(),
                reviewReport.isProcessed(),
                reviewReport.getReasonDetail(),
                reviewReport.getCreatedAt(),
                reviewReport.getUpdatedAt(),
                ReviewResponse.of(review, imageUrlList, templateChoiceResponseList)
        );
    }
}
