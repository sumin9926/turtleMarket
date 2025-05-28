package turtleMart.product.repository;

import com.querydsl.jpa.impl.JPAQueryFactory;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;
import turtleMart.global.utill.QueryDslSortUtil;
import turtleMart.member.entity.QSeller;
import turtleMart.product.entity.QRequestOptionGroup;
import turtleMart.product.entity.QRequestOptionValue;
import turtleMart.product.entity.RequestOptionGroup;
import turtleMart.product.entity.RequestOptionValueStatus;

import java.util.List;

@Repository
@RequiredArgsConstructor
public class RequestOptionGroupDslRepositoryImpl implements RequestOptionGroupDslRepository{

    private final JPAQueryFactory jpaQueryFactory;

    @Override
    public Page<RequestOptionGroup> findAllBySellerIdWithValue(Long sellerId, Pageable pageable) {
        QRequestOptionGroup requestOptionGroup = QRequestOptionGroup.requestOptionGroup;
        QRequestOptionValue value = QRequestOptionValue.requestOptionValue;
        QSeller seller = QSeller.seller;

        List<RequestOptionGroup> content = jpaQueryFactory
                .selectFrom(requestOptionGroup)
                .join(requestOptionGroup.requestOptionValueList, value).fetchJoin()
                .join(requestOptionGroup.seller,seller).fetchJoin()
                .where(requestOptionGroup.seller.id.eq(sellerId))
                .distinct()
                .offset(pageable.getOffset())
                .limit(pageable.getPageSize())
                .orderBy(QueryDslSortUtil.toOrderSpecifiers(pageable.getSort(), requestOptionGroup))
                .fetch();

        Long total = jpaQueryFactory
                .select(requestOptionGroup.count())
                .from(requestOptionGroup)
                .join(requestOptionGroup.requestOptionValueList, value)
                .join(requestOptionGroup.seller,seller)
                .where(requestOptionGroup.seller.id.eq(sellerId))
                .fetchOne();

        return new PageImpl<>(content, pageable, total);
    }

    @Override
    public Page<RequestOptionGroup> findAllByStatusWithSeller(Pageable pageable,RequestOptionValueStatus requestOptionValueStatus) {
        QRequestOptionGroup requestOptionGroup = QRequestOptionGroup.requestOptionGroup;
        QRequestOptionValue requestOptionValue = QRequestOptionValue.requestOptionValue;
        QSeller seller = QSeller.seller;

        List<RequestOptionGroup> content = jpaQueryFactory
                .selectFrom(requestOptionGroup)
                .join(requestOptionGroup.requestOptionValueList, requestOptionValue).fetchJoin()
                .join(requestOptionGroup.seller, seller).fetchJoin()
                .where(requestOptionValue.requestOptionValueStatus.eq(requestOptionValueStatus))
                .distinct()
                .offset(pageable.getOffset())
                .limit(pageable.getPageSize())
                .orderBy(QueryDslSortUtil.toOrderSpecifiers(pageable.getSort(), requestOptionGroup))
                .fetch();

        Long total = jpaQueryFactory
                .select(requestOptionGroup.count())
                .from(requestOptionGroup)
                .join(requestOptionGroup.requestOptionValueList,requestOptionValue)
                .join(requestOptionGroup.seller, seller)
                .where(requestOptionValue.requestOptionValueStatus.eq(RequestOptionValueStatus.PENDING))
                .fetchOne();

        return new PageImpl<>(content, pageable, total);
    }


    @Override
    public RequestOptionGroup findByIdWithValue(Long requestOptionGroupId) {
        QRequestOptionGroup requestOptionGroup = QRequestOptionGroup.requestOptionGroup;
        QRequestOptionValue requestOptionValue = QRequestOptionValue.requestOptionValue;
        return jpaQueryFactory.selectFrom(requestOptionGroup)
                .join(requestOptionGroup.requestOptionValueList,requestOptionValue).fetchJoin()
                .where(requestOptionGroup.id.eq(requestOptionGroupId))
                .fetchOne();
    }
}
