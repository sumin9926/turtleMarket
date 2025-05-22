package turtleMart.review.repository;

import turtleMart.review.entity.ReviewReport;

import java.util.Optional;

public interface ReviewReportDslRepository {

    Optional<ReviewReport> findByIdWithReportCode(Long reviewReportId);
}
