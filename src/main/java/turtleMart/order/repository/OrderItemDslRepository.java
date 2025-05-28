package turtleMart.order.repository;

import turtleMart.order.dto.response.RefundResponse;
import turtleMart.order.entity.OrderItemStatus;

import java.util.List;

public interface OrderItemDslRepository {
    List<RefundResponse> findAllOrderItemBySellerIdAndStatusRefunding(Long sellerId, OrderItemStatus status);
}
