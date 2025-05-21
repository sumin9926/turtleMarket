package turtleMart.review.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import turtleMart.review.entity.ReviewReport;

public interface ReviewReportRepository extends JpaRepository<ReviewReport, Long> {
}
