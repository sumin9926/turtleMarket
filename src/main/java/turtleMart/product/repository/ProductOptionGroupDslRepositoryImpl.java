package turtleMart.product.repository;

import com.querydsl.jpa.impl.JPAQueryFactory;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;
import turtleMart.global.utill.QueryDslSortUtil;
import turtleMart.product.entity.ProductOptionGroup;
import turtleMart.product.entity.QProductOptionGroup;
import turtleMart.product.entity.QProductOptionValue;

import java.util.List;

@Repository
@RequiredArgsConstructor
public class ProductOptionGroupDslRepositoryImpl implements ProductOptionGroupDslRepository{

    private final JPAQueryFactory jpaQueryFactory;

    @Override
    public Page<ProductOptionGroup> findAllWithValue(Pageable pageable) {
        QProductOptionGroup productOptionGroup = QProductOptionGroup.productOptionGroup;
        QProductOptionValue productOptionValue = QProductOptionValue.productOptionValue;

        List<ProductOptionGroup> content = jpaQueryFactory.selectFrom(productOptionGroup)
                .join(productOptionGroup.productOptionValueList, productOptionValue).fetchJoin()
                .distinct()
                .offset(pageable.getOffset())
                .limit(pageable.getPageSize())
                .orderBy(QueryDslSortUtil.toOrderSpecifiers(pageable.getSort(), productOptionGroup))
                .fetch();

        Long total = jpaQueryFactory.select(productOptionGroup.count())
                .from(productOptionGroup)
                .fetchOne();
        return new PageImpl<>(content,pageable,total);
    }
}
