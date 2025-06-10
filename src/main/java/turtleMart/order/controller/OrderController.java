package turtleMart.order.controller;

import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.context.request.async.DeferredResult;
import turtleMart.order.dto.request.CartOrderSheetRequest;
import turtleMart.order.dto.request.OrderItemStatusRequest;
import turtleMart.order.dto.request.OrderWrapperRequest;
import turtleMart.order.dto.response.MemberOrderListResponse;
import turtleMart.order.dto.response.OrderDetailResponse;
import turtleMart.order.dto.response.TotalOrderedQuantityResponse;
import turtleMart.order.service.OrderWaiter;
import turtleMart.order.service.OrderService;

import java.time.LocalDate;
import java.util.Arrays;
import java.util.List;

@RestController
@RequestMapping("/orders")
@RequiredArgsConstructor
public class OrderController {

    private final OrderService orderService;
    private final OrderWaiter orderWaiter;

    @PostMapping("/orders")
    public DeferredResult<ResponseEntity<OrderWrapperRequest>> createOrder(
            @RequestParam String items, //상품옵션Id1:수량,상품옵션Id2:수량 형식으로 입력
            @RequestBody OrderWrapperRequest request,
            @RequestHeader("Idempotency-key") String idempotencyKey
            /*TODO JWT 통해서 회원 ID 가져오기*/
    ) {
        List<CartOrderSheetRequest> productNameAndQuantityList = CartOrderSheetRequest.splitItemIdAndQuantity(items);

        DeferredResult<ResponseEntity<OrderWrapperRequest>> result = orderWaiter.createWaiter(idempotencyKey);
        orderService.tryOrder(1L, productNameAndQuantityList, request, idempotencyKey);

        return result; //ResponseEntity<OrderWrapperRequest>를 반환함
    }

    @GetMapping("/{orderId}")
    public ResponseEntity<OrderDetailResponse> getOrderDetail(
            @PathVariable Long orderId
            /*TODO JWT 통해서 회원 ID 가져오기*/
    ) {
        OrderDetailResponse response = orderService.getOrderDetail(1L, orderId);

        return ResponseEntity.status(HttpStatus.OK).body(response);
    }

    @PatchMapping("/{orderId}/order-items/{orderItemIds}")
    public ResponseEntity<OrderDetailResponse> updateOrderItemStatus(
            @PathVariable Long orderId,
            @PathVariable String orderItemIds, // 1,2,3 형태로 입력
            @Valid @RequestBody OrderItemStatusRequest request
            /*TODO JWT 통해서 회원 ID 가져오기*/
    ) {
        List<Long> orderItemIdList = Arrays.stream(orderItemIds.split(",")).map(Long::parseLong).toList();

        OrderDetailResponse response = orderService.updateOrderItemStatus(orderId, orderItemIdList, request, 1L);

        return ResponseEntity.status(HttpStatus.OK).body(response);
    }

    @GetMapping("/my")
    public ResponseEntity<MemberOrderListResponse> getMyOrderList(
            /*TODO JWT 통해서 회원 ID 가져오기*/
    ){
        MemberOrderListResponse response = orderService.getOrderList(1L);

        return ResponseEntity.status(HttpStatus.OK).body(response);
    }

    @GetMapping("/seller")
    public ResponseEntity<TotalOrderedQuantityResponse> getTotalOrderedQuantity(
            @RequestParam Long productId,
            @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate, //2025-05-10 형식으로 입력(이하 동일)
            @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate
            /*TODO JWT 통해서 판매자 ID 가져오기*/
    ){
        TotalOrderedQuantityResponse response = orderService.getTotalOrderedQuantity(1L, productId, startDate, endDate);

        return ResponseEntity.status(HttpStatus.OK).body(response);
    }
}
