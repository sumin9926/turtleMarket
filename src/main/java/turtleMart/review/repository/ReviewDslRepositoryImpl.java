package turtleMart.review.repository;

import com.querydsl.jpa.impl.JPAQueryFactory;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;
import turtleMart.review.entity.Review;
import turtleMart.review.entity.TemplateChoice;

import java.util.List;
import java.util.Optional;

import static turtleMart.review.entity.QProductReviewTemplate.productReviewTemplate;
import static turtleMart.review.entity.QReview.review;
import static turtleMart.review.entity.QReviewTemplate.reviewTemplate;
import static turtleMart.review.entity.QTemplateChoice.templateChoice;

@Repository
@RequiredArgsConstructor
public class ReviewDslRepositoryImpl implements ReviewDslRepository{

    private final JPAQueryFactory jpaQueryFactory;

    @Override
    public Optional<Review> findByIdWithChoice(Long reviewId) {
        List<TemplateChoice> templateChoiceList =jpaQueryFactory.select(templateChoice)
                .from(templateChoice)
                .join(templateChoice.review, review).fetchJoin()
                .join(templateChoice.productReviewTemplate, productReviewTemplate).fetchJoin()
                .join(productReviewTemplate.reviewTemplate, reviewTemplate).fetchJoin()
                .where(templateChoice.review.id.eq(reviewId))
                .fetch();

        return templateChoiceList.isEmpty() ? Optional.empty() : Optional.ofNullable(templateChoiceList.get(0).getReview());
    }
}
