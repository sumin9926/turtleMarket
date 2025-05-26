package turtleMart.review.repository;

import com.querydsl.jpa.impl.JPAQueryFactory;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;
import turtleMart.review.entity.ReviewReport;
import java.util.Optional;
import static turtleMart.review.entity.QProductReviewTemplate.productReviewTemplate;
import static turtleMart.review.entity.QReasonCode.reasonCode;
import static turtleMart.review.entity.QReview.review;
import static turtleMart.review.entity.QReviewReport.reviewReport;
import static turtleMart.review.entity.QReviewTemplate.reviewTemplate;
import static turtleMart.review.entity.QTemplateChoice.templateChoice;

@Repository
@RequiredArgsConstructor
public class ReviewReportDslRepositoryImpl implements ReviewReportDslRepository{

    private final JPAQueryFactory jpaQueryFactory;

    @Override
    public Optional<ReviewReport> findByIdWithReportCode(Long reviewReportId) {
        return Optional.ofNullable(jpaQueryFactory.selectFrom(reviewReport)
                .join(reviewReport.reasonCode, reasonCode).fetchJoin()
                .join(reviewReport.review, review).fetchJoin()
                .join(review.templateChoiceList, templateChoice).fetchJoin()
                .join(templateChoice.productReviewTemplate,productReviewTemplate).fetchJoin()
                .join(productReviewTemplate.reviewTemplate, reviewTemplate).fetchJoin()
                .where(reviewReport.id.eq(reviewReportId))
                .fetchOne());
    }
}
