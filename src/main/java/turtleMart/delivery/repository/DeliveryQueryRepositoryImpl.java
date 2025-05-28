package turtleMart.delivery.repository;

import com.querydsl.jpa.impl.JPAQueryFactory;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;
import turtleMart.delivery.entity.Delivery;
import turtleMart.delivery.entity.QDelivery;

import java.util.List;

@Repository
@RequiredArgsConstructor
public class DeliveryQueryRepositoryImpl implements DeliveryQueryRepository{

    private final JPAQueryFactory queryFactory;

    @Override
    public List<Delivery> findAllByMemberId(Long memberId) {
        QDelivery delivery = QDelivery.delivery;

        return queryFactory
            .selectFrom(delivery)
            .where(delivery.address.member.id.eq(memberId))
            .fetch();
    }

    @Override
    public List<Delivery> findAllBySeller(Long sellerId) {
        QDelivery delivery = QDelivery.delivery;

        return queryFactory
            .selectFrom(delivery)
            .where(delivery.seller.id.eq(sellerId))
            .fetch();
    }
}
