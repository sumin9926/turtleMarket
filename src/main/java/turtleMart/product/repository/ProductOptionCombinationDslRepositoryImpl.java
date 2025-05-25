package turtleMart.product.repository;

import com.querydsl.jpa.impl.JPAQueryFactory;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;
import turtleMart.product.entity.ProductOptionCombination;
import turtleMart.product.entity.QProductOptionCombination;
import turtleMart.product.entity.QProductOptionMap;
import turtleMart.product.entity.QProductOptionValue;

import java.util.List;

@Repository
@RequiredArgsConstructor
public class ProductOptionCombinationDslRepositoryImpl implements ProductOptionCombinationDslRepository{

    private final JPAQueryFactory jpaQueryFactory;

    @Override
    public List<ProductOptionCombination> findAllByProductIdWithMapAndValue(Long productId) {
        QProductOptionCombination productOptionCombination = QProductOptionCombination.productOptionCombination;
        QProductOptionMap productOptionMap = QProductOptionMap.productOptionMap;
        QProductOptionValue productOptionValue = QProductOptionValue.productOptionValue;
        return jpaQueryFactory.selectFrom(productOptionCombination)
                .join(productOptionCombination.productOptionMapList, productOptionMap).fetchJoin()
                .join(productOptionMap.productOptionValue, productOptionValue).fetchJoin()
                .where(productOptionCombination.product.id.eq(productId))
                .distinct()
                .fetch();
    }
}
