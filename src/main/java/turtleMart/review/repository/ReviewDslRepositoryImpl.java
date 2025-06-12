package turtleMart.review.repository;

import com.querydsl.jpa.impl.JPAQueryFactory;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;
import turtleMart.global.common.CursorPageResponse;
import turtleMart.review.entity.Review;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

import static turtleMart.review.entity.QProductReviewTemplate.productReviewTemplate;
import static turtleMart.review.entity.QReview.review;
import static turtleMart.review.entity.QReviewTemplate.reviewTemplate;
import static turtleMart.review.entity.QTemplateChoice.templateChoice;

@Repository
@RequiredArgsConstructor
public class ReviewDslRepositoryImpl implements ReviewDslRepository {

    private final JPAQueryFactory jpaQueryFactory;

    @Override
    public Optional<Review> findByIdWithChoice(Long reviewId) {
        return Optional.ofNullable(
                jpaQueryFactory.select(review).distinct().from(review)
                        .leftJoin(review.templateChoiceList, templateChoice).fetchJoin()
                        .leftJoin(templateChoice.productReviewTemplate, productReviewTemplate).fetchJoin()
                        .leftJoin(productReviewTemplate.reviewTemplate, reviewTemplate).fetchJoin()
                        .where(review.id.eq(reviewId))
                        .fetchOne());
    }

    @Override
    public Page<Review> findByMemberIdWithPagination(Long memberId, Pageable pageable) {

        int offset = pageable.getPageSize() * pageable.getPageNumber();
        int limit = pageable.getPageSize();

        List<Review> reviewList =
                jpaQueryFactory.select(review).distinct().from(review)
                        .leftJoin(review.templateChoiceList, templateChoice).fetchJoin()
                        .leftJoin(templateChoice.productReviewTemplate, productReviewTemplate).fetchJoin()
                        .leftJoin(productReviewTemplate.reviewTemplate, reviewTemplate).fetchJoin()
                        .where(review.member.id.eq(memberId))
                        .orderBy(review.id.desc())
                        .offset(offset)
                        .limit(limit)
                        .fetch();

        Long count = jpaQueryFactory
                .select(review.count())
                .from(review)
                .where(review.member.id.eq(memberId))
                .fetchOne();

        return new PageImpl<>(reviewList, pageable, count != null ? count : 0);
    }

    @Override
    public List<Review> findByIdInWithPagination(List<Long> reviewIdList, int size) {


        List<Review> reviewList = jpaQueryFactory.select(review).distinct().from(review)
                .leftJoin(review.templateChoiceList, templateChoice).fetchJoin()
                .leftJoin(templateChoice.productReviewTemplate, productReviewTemplate).fetchJoin()
                .leftJoin(productReviewTemplate.reviewTemplate, reviewTemplate).fetchJoin()
                .where(review.id.in(reviewIdList))
                .limit(size + 1)
                .orderBy(review.id.desc())
                .fetch();

        return reviewList;
        }

    @Override
    public CursorPageResponse<Review> findAllPendingSync(LocalDateTime lastSyncedAt, LocalDateTime startSyncTime, Long lastCursor) {
        List<Review> reviewList = jpaQueryFactory.select(review).from(review)
                .where(
                        review.updatedAt.between(lastSyncedAt, startSyncTime),
                        review.syncRequired.eq(true),
                        review.isDeleted.eq(false),
                        review.id.gt(lastCursor)
                )
                .orderBy(review.id.desc())
                .limit(1000)
                .fetch();

        Long cursor = reviewList.isEmpty() ? 0 : reviewList.get(reviewList.size() - 1).getId();

        return CursorPageResponse.of(reviewList, cursor, true);
    }

    @Override
    public void updateSyncStatus(List<Long> reviewIdList) {
        jpaQueryFactory.update(review)
                .set(review.syncRequired, false)
                .where(review.id.in(reviewIdList))
                .execute();
    }

//    private Predicate fullTextQuery(String keyword) {
//        NumberTemplate<Double> score = Expressions.numberTemplate(
//                Double.class, "function('match',{0},{1},{2})", review.title, review.content, Expressions.constant(keyword));
//
//        return score.gt(0);
//    }
}
