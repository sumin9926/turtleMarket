package turtleMart.order.dto.response;

import turtleMart.delivery.entity.Delivery;
import turtleMart.delivery.entity.DeliveryStatus;
import turtleMart.order.entity.Order;

import java.time.LocalDateTime;
import java.util.List;

public record OrderSimpleResponse(
        Long orderId,
        DeliveryStatus deliveryStatus,
        List<OrderItemResponse> orderItems,
        LocalDateTime orderedAt
) {
    public static OrderSimpleResponse from(Order order, Delivery delivery, List<OrderItemResponse> orderItems){
        return new OrderSimpleResponse(
                order.getId(),
                delivery.getDeliveryStatus(),
                orderItems,
                order.getOrderedAt()
        );
    }
}
