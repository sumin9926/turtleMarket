package turtleMart.review.repository;

import com.querydsl.core.BooleanBuilder;
import com.querydsl.jpa.impl.JPAQueryFactory;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Repository;
import turtleMart.review.entity.ReviewReport;
import turtleMart.review.entity.ReviewReportStatus;

import java.util.List;
import java.util.Optional;

import static turtleMart.review.entity.QProductReviewTemplate.productReviewTemplate;
import static turtleMart.review.entity.QReasonCode.reasonCode;
import static turtleMart.review.entity.QReview.review;
import static turtleMart.review.entity.QReviewReport.reviewReport;
import static turtleMart.review.entity.QReviewTemplate.reviewTemplate;
import static turtleMart.review.entity.QTemplateChoice.templateChoice;


@Slf4j
@Repository
@RequiredArgsConstructor
public class ReviewReportDslRepositoryImpl implements ReviewReportDslRepository {

    private final JPAQueryFactory jpaQueryFactory;

    @Override
    public Optional<ReviewReport> findByIdWithReportCode(Long reviewReportId) {
        Optional<ReviewReport> optionalReviewReport = Optional.ofNullable(
                jpaQueryFactory.select(reviewReport)
                        .distinct()
                        .from(reviewReport)
                        .join(reviewReport.reasonCode, reasonCode).fetchJoin()
                        .join(reviewReport.review, review).fetchJoin()
                        .leftJoin(review.templateChoiceList, templateChoice).fetchJoin()
                        .leftJoin(templateChoice.productReviewTemplate, productReviewTemplate).fetchJoin()
                        .leftJoin(productReviewTemplate.reviewTemplate, reviewTemplate).fetchJoin()
                        .where(reviewReport.id.eq(reviewReportId))
                        .fetchOne());

        return optionalReviewReport;
    }

    @Override
    public List<ReviewReport> findByReviewReportCondition(String reviewReportStatus, String reviewReasonCode, Long cursor) {
        BooleanBuilder builder = new BooleanBuilder();

        if (reviewReportStatus != null) {
            ReviewReportStatus status = ReviewReportStatus.of(reviewReportStatus);
            builder.and(reviewReport.reviewReportStatus.eq(status));
        }

        if (reviewReasonCode != null) {
            builder.and(reviewReport.reasonCode.reason.eq(reviewReasonCode));
        }

        if (cursor != null) {
            builder.and(reviewReport.id.gt(cursor));
        }

        return jpaQueryFactory.select(reviewReport)
                        .from(reviewReport)
                        .join(reviewReport.reasonCode, reasonCode).fetchJoin()
                        .join(reviewReport.review, review).fetchJoin()
                        .leftJoin(review.templateChoiceList, templateChoice).fetchJoin()
                        .leftJoin(templateChoice.productReviewTemplate, productReviewTemplate).fetchJoin()
                        .leftJoin(productReviewTemplate.reviewTemplate, reviewTemplate).fetchJoin()
                        .where(builder)
                        .limit(10)
                        .orderBy(reviewReport.id.desc())
                        .fetch();
    }
}
