package turtleMart.order.repository;

import com.querydsl.core.types.Projections;
import com.querydsl.jpa.impl.JPAQueryFactory;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;
import turtleMart.global.common.OptionDisplayUtils;
import turtleMart.member.entity.QMember;
import turtleMart.order.dto.response.RefundResponse;
import turtleMart.order.entity.OrderItemStatus;
import turtleMart.order.entity.QOrder;
import turtleMart.order.entity.QOrderItem;
import turtleMart.product.entity.QProduct;
import turtleMart.product.entity.QProductOptionCombination;
import turtleMart.product.repository.ProductOptionValueRepository;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

@Repository
@RequiredArgsConstructor
public class OrderItemDslRepositoryImpl implements OrderItemDslRepository{

    QOrderItem orderItem = QOrderItem.orderItem;
    QProduct product = QProduct.product;
    QOrder order = QOrder.order;
    QMember member = QMember.member;
    QProductOptionCombination combination = QProductOptionCombination.productOptionCombination;
    private final JPAQueryFactory queryFactory;
    private final ProductOptionValueRepository productOptionValueRepository;

    @Override
    public List<RefundResponse> findAllOrderItemBySellerIdAndStatusRefunding(Long sellerId, OrderItemStatus status) {
        List<RefundResponse> resultList =  queryFactory
                .select(Projections.constructor(
                        RefundResponse.class,
                        member.id,
                        order.id,
                        orderItem.id,
                        orderItem.name,
                        combination.uniqueKey, //임시로 uniqueKey 저장
                        orderItem.quantity,
                        orderItem.price,
                        orderItem.quantity.multiply(orderItem.price) // totalPrice
                ))
                .from(orderItem)
                .join(orderItem.order, order)
                .join(order.member, member)
                .join(orderItem.productOptionCombination.product, product)
                .join(orderItem.productOptionCombination, combination)
                .where(
                        product.seller.id.eq(sellerId),
                        orderItem.orderItemStatus.eq(status)
                )
                .fetch();

        Set<String> uniqueKeySet = resultList.stream()
                .map(RefundResponse::optionInfo)
                .collect(Collectors.toSet());

        Map<String, String> optionDisplayMap = OptionDisplayUtils.buildOptionDisplayMap(uniqueKeySet, productOptionValueRepository);

        return resultList.stream()
                .map(r -> new RefundResponse(
                        r.memberId(),
                        r.orderId(),
                        r.orderItemId(),
                        r.productName(),
                        optionDisplayMap.getOrDefault(r.optionInfo(), "(옵션 정보 없음)"), // uniqueKey → optionInfo 변환
                        r.quantity(),
                        r.price(),
                        r.totalPrice()
                ))
                .toList();
    }

    @Override
    public Long getTotalOrderedQuantity(
            Long sellerId, Long productId, LocalDateTime startDateTime, LocalDateTime endDateTime, OrderItemStatus status
    ) {
       return queryFactory
                .select(orderItem.quantity.sumLong().coalesce(0L))
                .from(orderItem)
                .join(orderItem.productOptionCombination, combination)
                .join(combination.product, product)
                .join(orderItem.order, order)
                .where(
                        product.id.eq(productId),
                        product.seller.id.eq(sellerId),
                        order.orderedAt.goe(startDateTime),
                        order.orderedAt.lt(endDateTime),
                        orderItem.orderItemStatus.ne(status)
                )
                .fetchOne();
    }
}
