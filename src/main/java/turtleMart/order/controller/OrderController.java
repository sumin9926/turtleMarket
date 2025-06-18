package turtleMart.order.controller;

import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.context.request.async.DeferredResult;
import turtleMart.global.exception.ErrorCode;
import turtleMart.global.exception.ForbiddenException;
import turtleMart.member.entity.Authority;
import turtleMart.order.dto.request.CartOrderSheetRequest;
import turtleMart.order.dto.request.OrderItemStatusRequest;
import turtleMart.order.dto.request.OrderWrapperRequest;
import turtleMart.order.dto.response.MemberOrderListResponse;
import turtleMart.order.dto.response.OrderDetailResponse;
import turtleMart.order.dto.response.TotalOrderedQuantityResponse;
import turtleMart.order.service.OrderWaiter;
import turtleMart.order.service.OrderService;
import turtleMart.security.AuthUser;

import java.time.LocalDate;
import java.util.Arrays;
import java.util.List;

@RestController
@RequestMapping("/orders")
@RequiredArgsConstructor
public class OrderController {

    private final OrderService orderService;
    private final OrderWaiter orderWaiter;

    @PostMapping("/async")
    public DeferredResult<ResponseEntity<OrderWrapperRequest>> createOrder(
            @RequestParam String items, //상품옵션Id1:수량,상품옵션Id2:수량 형식으로 입력
            @RequestBody OrderWrapperRequest request,
            @RequestHeader("Idempotency-key") String orderKey,
            @AuthenticationPrincipal AuthUser authUser
    ) {
        List<CartOrderSheetRequest> productNameAndQuantityList = CartOrderSheetRequest.splitItemIdAndQuantity(items);

        DeferredResult<ResponseEntity<OrderWrapperRequest>> result = orderWaiter.createWaiter(orderKey);
        orderService.tryOrder(authUser.memberId(), productNameAndQuantityList, request, orderKey);

        return result; //ResponseEntity<OrderWrapperRequest>를 반환함
    }

    @GetMapping("/{orderId}")
    public ResponseEntity<OrderDetailResponse> getOrderDetail(
            @PathVariable Long orderId,
            @AuthenticationPrincipal AuthUser authUser
    ) {
        OrderDetailResponse response = orderService.getOrderDetail(authUser.memberId(), orderId);

        return ResponseEntity.status(HttpStatus.OK).body(response);
    }

    @PatchMapping("/{orderId}/order-items/{orderItemIds}")
    public ResponseEntity<OrderDetailResponse> updateOrderItemStatus(
            @PathVariable Long orderId,
            @PathVariable String orderItemIds, // 1,2,3 형태로 입력
            @Valid @RequestBody OrderItemStatusRequest request,
            @AuthenticationPrincipal AuthUser authUser
    ) {
        List<Long> orderItemIdList = Arrays.stream(orderItemIds.split(",")).map(Long::parseLong).toList();

        OrderDetailResponse response = orderService.updateOrderItemStatus(orderId, orderItemIdList, request, authUser.memberId());

        return ResponseEntity.status(HttpStatus.OK).body(response);
    }

    @GetMapping("/my")
    public ResponseEntity<MemberOrderListResponse> getMyOrderList( // 배송 테이블까지 생성되어야 정상적으로 실행 됨
            @AuthenticationPrincipal AuthUser authUser
    ){
        MemberOrderListResponse response = orderService.getOrderList(authUser.memberId());

        return ResponseEntity.status(HttpStatus.OK).body(response);
    }

    @GetMapping("/seller")
    public ResponseEntity<TotalOrderedQuantityResponse> getTotalOrderedQuantity(
            @RequestParam Long productId,
            @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate, //2025-05-10 형식으로 입력(이하 동일)
            @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @AuthenticationPrincipal AuthUser authUser
    ){
        if(!authUser.hasAuthority(Authority.SELLER)){
            throw new ForbiddenException(ErrorCode.FORBIDDEN);
        }
        TotalOrderedQuantityResponse response = orderService.getTotalOrderedQuantity(authUser.memberId(), productId, startDate, endDate);

        return ResponseEntity.status(HttpStatus.OK).body(response);
    }
}
