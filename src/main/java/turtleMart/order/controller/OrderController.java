package turtleMart.order.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import turtleMart.order.dto.response.OrderDetailResponse;
import turtleMart.order.service.OrderService;

@RestController
@RequestMapping("/orders")
@RequiredArgsConstructor
public class OrderController {

    private final OrderService orderService;

    @GetMapping("/{orderId}")
    public ResponseEntity<OrderDetailResponse> getOrderDetail(
            @PathVariable Long orderId
            /*TODO JWT 통해서 회원 ID 가져오기*/
    ) {
        OrderDetailResponse response = orderService.getOrderDetail(1L, orderId);

        return ResponseEntity.status(HttpStatus.OK).body(response);
    }
}
