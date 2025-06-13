package turtleMart.review.repository;

import turtleMart.review.entity.ReviewReport;

import java.util.List;
import java.util.Optional;

public interface ReviewReportDslRepository {

    Optional<ReviewReport> findByIdWithReportCode(Long reviewReportId);

    List<ReviewReport> findByReviewReportCondition(String reviewReportStatus, String reasonCode, Long cursor);
}
