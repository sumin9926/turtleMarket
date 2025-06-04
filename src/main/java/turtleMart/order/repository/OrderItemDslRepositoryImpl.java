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
        List<RefundResponse> results =  queryFactory
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

        return results.stream()
                .map(r -> new RefundResponse(
                        r.memberId(),
                        r.orderId(),
                        r.orderItemId(),
                        r.productName(),
                        OptionDisplayUtils.buildOptionDisplay(r.optionInfo(), productOptionValueRepository), // uniqueKey → optionInfo 변환
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
       Long total = queryFactory
                .select(orderItem.quantity.sum().longValue())
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

       return total != null ? total : 0L;
    }
}
