package turtleMart.order.repository;

import org.springframework.data.repository.query.Param;
import turtleMart.order.dto.response.RefundResponse;
import turtleMart.order.entity.OrderItemStatus;

import java.time.LocalDateTime;
import java.util.List;

public interface OrderItemDslRepository {
    List<RefundResponse> findAllOrderItemBySellerIdAndStatusRefunding(Long sellerId, OrderItemStatus status);

    Long getTotalOrderedQuantity(
            @Param("sellerId") Long sellerId, @Param("productId") Long productId,
            @Param("startDate") LocalDateTime startDateTime, @Param("endDate") LocalDateTime endDateTime,
            @Param("status") OrderItemStatus status
    );
}
