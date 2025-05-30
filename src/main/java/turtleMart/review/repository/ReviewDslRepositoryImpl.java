package turtleMart.review.repository;

import com.querydsl.core.BooleanBuilder;
import com.querydsl.core.types.Expression;
import com.querydsl.core.types.Predicate;
import com.querydsl.core.types.dsl.Expressions;
import com.querydsl.core.types.dsl.NumberTemplate;
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
        List<TemplateChoice> templateChoiceList = jpaQueryFactory.select(templateChoice)
                .from(templateChoice)
                .join(templateChoice.review, review).fetchJoin()
                .join(templateChoice.productReviewTemplate, productReviewTemplate).fetchJoin()
                .join(productReviewTemplate.reviewTemplate, reviewTemplate).fetchJoin()
                .where(templateChoice.review.id.eq(reviewId))
                .fetch();

        return templateChoiceList.isEmpty() ? Optional.empty() : Optional.ofNullable(templateChoiceList.get(0).getReview());
    }

    @Override
    public Page<Review> findByMemberIdWithPagination(Long memberId, Pageable pageable) {
        List<Review> reviewList = jpaQueryFactory.select(templateChoice)
                .from(templateChoice)
                .join(templateChoice.review, review).fetchJoin()
                .join(templateChoice.productReviewTemplate, productReviewTemplate).fetchJoin()
                .join(productReviewTemplate.reviewTemplate, reviewTemplate).fetchJoin()
                .where(review.member.id.eq(memberId))
                .orderBy(review.id.desc())
                .offset(pageable.getOffset())
                .limit(pageable.getPageSize())
                .fetch().stream()
                .map(TemplateChoice::getReview)
                .toList();

        Long count = jpaQueryFactory
                .select(review.count())
                .from(review)
                .where(review.member.id.eq(memberId))
                .fetchOne();

        return new PageImpl<>(reviewList, pageable, count);
    }

    @Override // mysql fulltext사용
    public List<Review> findByProductWithSearch(Long productId, String keyWord, Integer rating) {
        //키워드를 가지고 리뷰의 제목과 컨텐츠를 풀텍스트 인덱스를 사용하여 검색해야함,
        //rating을 이용해서 별점만 구분

        BooleanBuilder booleanBuilder = new BooleanBuilder();

        booleanBuilder.and(review.product.id.eq(productId));

        if(keyWord != null && !keyWord.isEmpty()){
            booleanBuilder.and(fullTextQuery(keyWord));
        }

        if(rating != null){booleanBuilder.and(review.rating.eq(rating));}

        return jpaQueryFactory.select(review)
                .from(review)
                .leftJoin(review.templateChoiceList, templateChoice).fetchJoin()
                .leftJoin(templateChoice.productReviewTemplate).fetchJoin()
                .leftJoin(productReviewTemplate.reviewTemplate, reviewTemplate).fetchJoin()
                .where(booleanBuilder)
                .offset(0)
                .limit(10)
                .fetch();
    }

    private Predicate fullTextQuery(String keyword){
        NumberTemplate<Double> score = Expressions.numberTemplate(
                Double.class, "function('match',{0},{1},{2})", review.title, review.content,  Expressions.constant(keyword));

        return score.gt(0);
    }
}
