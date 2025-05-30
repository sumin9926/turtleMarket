package turtleMart.order.dto.request;

import jakarta.validation.constraints.NotNull;
import turtleMart.delivery.dto.reqeust.CreateDeliveryRequest;
import turtleMart.payment.dto.request.PaymentRequest;

import java.util.List;

public record OrderWrapperRequest(
        @NotNull(message = "주문상품 관련 정보 입력은 필수입니다.")
        List<OrderRequest> orderList,
        List<CartOrderSheetRequest> itemList, //정보 입력 불필요

        @NotNull(message = "결재 관련 정보 입력은 필수입니다.")
        PaymentRequest payment,

        @NotNull(message = "배송 관련 정보 입력은 필수입니다.")
        CreateDeliveryRequest delivery
) {
        public static OrderWrapperRequest updateItemList(OrderWrapperRequest wrapperRequest, List<CartOrderSheetRequest> itemList){
                return new OrderWrapperRequest(
                        wrapperRequest.orderList,
                        itemList,
                        wrapperRequest.payment,
                        wrapperRequest.delivery
                );
        }
}
