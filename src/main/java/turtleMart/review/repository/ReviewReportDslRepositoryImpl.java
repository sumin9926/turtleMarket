package turtleMart.review.repository;

import com.fasterxml.jackson.core.type.TypeReference;
import com.querydsl.core.BooleanBuilder;
import com.querydsl.core.types.dsl.BooleanExpression;
import com.querydsl.jpa.impl.JPAQueryFactory;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;
import turtleMart.global.common.CursorPageResponse;
import turtleMart.global.utill.JsonHelper;
import turtleMart.review.dto.response.ReviewReportResponse;
import turtleMart.review.dto.response.TemplateChoiceResponse;
import turtleMart.review.entity.Review;
import turtleMart.review.entity.ReviewReport;
import turtleMart.review.entity.ReviewReportStatus;
import turtleMart.review.entity.TemplateChoice;

import java.util.List;
import java.util.Optional;

import static turtleMart.review.entity.QProductReviewTemplate.productReviewTemplate;
import static turtleMart.review.entity.QReasonCode.reasonCode;
import static turtleMart.review.entity.QReview.review;
import static turtleMart.review.entity.QReviewReport.reviewReport;
import static turtleMart.review.entity.QReviewTemplate.reviewTemplate;
import static turtleMart.review.entity.QTemplateChoice.templateChoice;

@Repository
@RequiredArgsConstructor
public class ReviewReportDslRepositoryImpl implements ReviewReportDslRepository {

    private final JPAQueryFactory jpaQueryFactory;

    @Override
    public Optional<ReviewReport> findByIdWithReportCode(Long reviewReportId) {
        return Optional.ofNullable(jpaQueryFactory.selectFrom(reviewReport)
                .join(reviewReport.reasonCode, reasonCode).fetchJoin()
                .join(reviewReport.review, review).fetchJoin()
                .join(review.templateChoiceList, templateChoice).fetchJoin()
                .join(templateChoice.productReviewTemplate, productReviewTemplate).fetchJoin()
                .join(productReviewTemplate.reviewTemplate, reviewTemplate).fetchJoin()
                .where(reviewReport.id.eq(reviewReportId))
                .fetchOne());
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

        List<ReviewReport> reviewReportList = jpaQueryFactory.selectFrom(reviewReport)
                .join(reviewReport.reasonCode, reasonCode).fetchJoin()
                .join(reviewReport.review, review).fetchJoin()
                .join(review.templateChoiceList, templateChoice).fetchJoin()
                .join(templateChoice.productReviewTemplate, productReviewTemplate).fetchJoin()
                .join(productReviewTemplate.reviewTemplate, reviewTemplate).fetchJoin()
                .where(builder)
                .limit(10)
                .fetch();

       return reviewReportList;
    }
}
