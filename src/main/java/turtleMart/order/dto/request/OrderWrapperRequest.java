package turtleMart.order.dto.request;

import jakarta.validation.constraints.NotNull;
import turtleMart.delivery.dto.reqeust.CreateDeliveryRequest;
import turtleMart.payment.dto.request.PaymentRequest;

import java.util.List;

public record OrderWrapperRequest(
        String orderKey, //정보 입력 불필요. tryOrder 에서 업데이트 할 예정
        @NotNull(message = "주문상품 관련 정보 입력은 필수입니다.")
        List<OrderRequest> orderList,
        List<CartOrderSheetRequest> itemList, //정보 입력 불필요. tryOrder 에서 업데이트 할 예정

        @NotNull(message = "결재 관련 정보 입력은 필수입니다.")
        PaymentRequest payment,

        @NotNull(message = "배송 관련 정보 입력은 필수입니다.")
        CreateDeliveryRequest delivery
) {
        public static OrderWrapperRequest updateOrderKeyAndItemList(
                OrderWrapperRequest wrapperRequest, List<CartOrderSheetRequest> itemList, String orderKey
        ){
                return new OrderWrapperRequest(
                        orderKey,
                        wrapperRequest.orderList(),
                        itemList,
                        wrapperRequest.payment(),
                        wrapperRequest.delivery()
                );
        }
}
