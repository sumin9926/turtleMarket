package turtleMart.review.repository;

import com.querydsl.jpa.impl.JPAQueryFactory;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;
import turtleMart.review.entity.ProductReviewTemplate;

import java.util.List;
import java.util.Optional;

import static turtleMart.review.entity.QProductReviewTemplate.productReviewTemplate;
import static turtleMart.review.entity.QReviewTemplate.reviewTemplate;

@Repository
@RequiredArgsConstructor
public class ProductReviewTemplateDslRepositoryImpl implements ProductReviewTemplateDslRepository{

    private final JPAQueryFactory jpaQueryFactory;


    @Override
    public List<ProductReviewTemplate> findByIdInWithReviewTemplate(List<Long> productReviewTemplateIdList) {
        return jpaQueryFactory.selectDistinct(productReviewTemplate)
                .from(productReviewTemplate)
                .join(productReviewTemplate.reviewTemplate, reviewTemplate).fetchJoin()
                .where(productReviewTemplate.id.in(productReviewTemplateIdList))
                .fetch();
    }

    @Override
    public Optional<ProductReviewTemplate> findByIdWithReviewTemplate(Long productReviewTemplateId) {
        return Optional.ofNullable(jpaQueryFactory.selectDistinct(productReviewTemplate)
                .from(productReviewTemplate)
                .join(productReviewTemplate.reviewTemplate, reviewTemplate).fetchJoin()
                .where(productReviewTemplate.id.eq(productReviewTemplateId))
                .fetchOne());
    }
}
