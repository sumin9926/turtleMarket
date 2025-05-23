package turtleMart.order.dto.response;

import turtleMart.delivery.entity.Delivery;
import turtleMart.delivery.entity.DeliveryStatus;
import turtleMart.order.entity.Order;

import java.time.LocalDateTime;
import java.util.List;

public record OrderSimpleResponse(
        Long orderId,
        DeliveryStatus deliveryStatus,
        List<Long> orderItemIdList,
        LocalDateTime orderedAt
) {
    public static OrderSimpleResponse from(Order order, Delivery delivery, List<Long> orderItemIdList){
        return new OrderSimpleResponse(
                order.getId(),
                delivery.getDeliveryStatus(),
                orderItemIdList,
                order.getOrderAt()
        );
    }
}
