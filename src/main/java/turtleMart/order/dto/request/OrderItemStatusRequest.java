package turtleMart.order.dto.request;

import jakarta.validation.constraints.NotBlank;
import turtleMart.order.entity.OrderItemStatus;

import java.util.Arrays;

public record OrderItemStatusRequest(
        @NotBlank(message = "주문 상태 입력은 필수입니다.")
        String orderItemStatus
) {
    public static OrderItemStatus checkOrderItemStatusIgnoreCase(String orderItemStatus) {
        return Arrays.stream(OrderItemStatus.values())
                .filter(status -> status.name().equalsIgnoreCase(orderItemStatus))
                .findFirst()
                .orElseThrow(() -> new IllegalArgumentException("유효하지 않은 주문 상태값 입니다.(오탈자를 확인하세요): " + orderItemStatus));
    }
}
