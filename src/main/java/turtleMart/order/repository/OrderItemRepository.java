package turtleMart.order.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import turtleMart.order.entity.OrderItem;
import turtleMart.order.entity.OrderItemStatus;

import java.time.LocalDateTime;

public interface OrderItemRepository extends JpaRepository<OrderItem, Long> {

    /*TODO DSL로 변경 예정*/
    @Query(value = """
    SELECT SUM(oi.quantity)
    FROM order_item oi
    JOIN product_option_combination poc ON oi.product_option_combination_id = poc.id
    JOIN product p ON poc.product_id = p.id
    WHERE p.id = :productId
    AND p.seller_id = :sellerId
    AND oi.order_id IN (
        SELECT o.id FROM orders o
        WHERE o.ordered_at >= :startDate
        AND o.ordered_at < :endDate
    )
    AND oi.order_item_status <> :status
""", nativeQuery = true)
    Long countTotalOrderedBySellerAndProduct(
            @Param("sellerId")Long sellerId, @Param("productId")Long productId,
            @Param("startDate")LocalDateTime startDateTime, @Param("endDate")LocalDateTime endDateTime,
            @Param("status")OrderItemStatus status
            );
}
