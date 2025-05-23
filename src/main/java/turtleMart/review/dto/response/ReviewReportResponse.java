package turtleMart.review.dto.response;

import turtleMart.review.entity.ReviewReport;

import java.time.LocalDateTime;

public record ReviewReportResponse(Long id,
                                   Long memberId,
                                   String reasonCode,
                                   Boolean isProcessed,
                                   String ReasonDetail,
                                   LocalDateTime createdAt,
                                   LocalDateTime updatedAt,
                                   ReviewResponse reviewResponse
                                   ) {
    public static ReviewReportResponse of(ReviewResponse reviewResponse, ReviewReport reviewReport) {
        return new ReviewReportResponse(
                reviewReport.getId(),
                reviewReport.getMember().getId(),
                reviewReport.getReasonCode().getReason(),
                reviewReport.isProcessed(),
                reviewReport.getReasonDetail(),
                reviewReport.getCreatedAt(),
                reviewReport.getUpdatedAt(),
                reviewResponse
        );
    }
}
