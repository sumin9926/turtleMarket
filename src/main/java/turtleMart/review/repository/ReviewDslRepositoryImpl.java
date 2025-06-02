package turtleMart.review.repository;

import com.querydsl.jpa.impl.JPAQueryFactory;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;
import turtleMart.review.dto.response.ReviewResponse;
import turtleMart.review.entity.Review;
import turtleMart.review.entity.TemplateChoice;

import java.util.List;
import java.util.Optional;

import static turtleMart.review.entity.QProductReviewTemplate.productReviewTemplate;
import static turtleMart.review.entity.QReview.review;
import static turtleMart.review.entity.QReviewTemplate.reviewTemplate;
import static turtleMart.review.entity.QTemplateChoice.templateChoice;

@Slf4j
@Repository
@RequiredArgsConstructor
public class ReviewDslRepositoryImpl implements ReviewDslRepository {

    private final JPAQueryFactory jpaQueryFactory;

    @Override
    public Optional<Review> findByIdWithChoice(Long reviewId) {
        return Optional.ofNullable(
                jpaQueryFactory.select(review).distinct()
                        .from(review)
                        .leftJoin(review.templateChoiceList, templateChoice).fetchJoin()
                        .leftJoin(templateChoice.productReviewTemplate, productReviewTemplate).fetchJoin()
                        .leftJoin(productReviewTemplate.reviewTemplate, reviewTemplate).fetchJoin()
                        .where(review.id.eq(reviewId))
                        .fetchOne());
    }

    @Override
    public Page<Review> findByMemberIdWithPagination(Long memberId, Pageable pageable) {
        List<Review> reviewList =
                jpaQueryFactory.select(review)
                        .distinct()
                        .from(review)
                        .leftJoin(review.templateChoiceList, templateChoice).fetchJoin()
                        .leftJoin(templateChoice.productReviewTemplate, productReviewTemplate).fetchJoin()
                        .leftJoin(productReviewTemplate.reviewTemplate, reviewTemplate).fetchJoin()
                        .where(review.member.id.eq(memberId))
                        .orderBy(review.id.desc())
                        .offset(pageable.getPageSize() * pageable.getPageNumber())
                        .limit(pageable.getPageSize())
                        .fetch();

        log.info("리스트 크기:" +reviewList.size());


        Long count = jpaQueryFactory
                .select(review.count())
                .from(review)
                .where(review.member.id.eq(memberId))
                .fetchOne();

        return new PageImpl<>(reviewList, pageable, count != null ? count : 0);
    }


}
