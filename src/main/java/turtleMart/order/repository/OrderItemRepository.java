package turtleMart.order.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import turtleMart.order.entity.OrderItem;

import java.time.LocalDateTime;

public interface OrderItemRepository extends JpaRepository<OrderItem, Long> {

    @Query("""
        SELECT SUM(oi.quantity)
        FROM OrderItem oi
        WHERE oi.product.id = :productId
        AND oi.product.seller.id = :sellerId
        AND oi.order.orderedAt >= :startDate
        AND oi.order.orderedAt < :endDate
    """)
    Integer countTotalOrderedBySellerAndProduct(
            @Param("sellerId")Long sellerId, @Param("productId")Long productId,
            @Param("startDate")LocalDateTime startDateTime, @Param("endDate")LocalDateTime endDateTime
    );

    boolean existsByProductOptionCombinationId(Long productOptionCombinationId);
}
