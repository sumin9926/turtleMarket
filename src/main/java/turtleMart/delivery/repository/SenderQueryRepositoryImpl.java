package turtleMart.delivery.repository;

import com.querydsl.jpa.impl.JPAQueryFactory;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;
import turtleMart.delivery.entity.Courier;
import turtleMart.delivery.entity.QSender;

import java.util.Optional;

@Repository
@RequiredArgsConstructor
public class SenderQueryRepositoryImpl implements SenderQueryRepository{

    private final JPAQueryFactory queryFactory;

    @Override
    public long countByCourier(Courier courier) {
        QSender sender = QSender.sender;

        return Optional.ofNullable(
            queryFactory
            .select(sender.count())
            .from(sender)
            .where(sender.courier.eq(courier), sender.isDeleted.isFalse())
            .fetchOne()
        ).orElse(0L);
    }
}
