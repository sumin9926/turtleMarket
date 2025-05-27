package turtleMart.order.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import turtleMart.order.dto.request.CartOrderSheetRequest;
import turtleMart.order.dto.response.OrderSheetResponse;
import turtleMart.order.service.OrderService;

import java.util.List;

@RestController
@RequestMapping("/order-sheets")
@RequiredArgsConstructor
public class OrderSheetController {
    private final OrderService orderService;

    @GetMapping("/carts")
    public ResponseEntity<List<OrderSheetResponse>> getCartOrderSheet(
            @RequestParam String cartItems // 상품옵션Id1:수량,상품옵션Id2:수량 형식으로 입력 (UI와 Redis 싱크가 안 맞을 수 있기 때문에 사용자 화면을 그대로 전송 받음)
            /*TODO JWT 통해서 회원 ID 가져오기*/
    ) {
        List<CartOrderSheetRequest> orderSheetRequestList = CartOrderSheetRequest.splitItemIdAndQuantity(cartItems);
        List<OrderSheetResponse> responseList = orderService.getOrderSheet(orderSheetRequestList, 1L);

        return ResponseEntity.status(HttpStatus.OK).body(responseList);
    }

    @GetMapping("/direct")
    public ResponseEntity<OrderSheetResponse> getDirectOrderSheet(
            @RequestParam String productInfo // 상품옵션Id1:수량 형식으로 입력
            /*TODO JWT 통해서 회원 ID 가져오기*/
    ) {
        List<CartOrderSheetRequest> request = CartOrderSheetRequest.splitItemIdAndQuantity(productInfo);

        OrderSheetResponse response = orderService.getDirectOrderSheet(request, 1L);

        return ResponseEntity.status(HttpStatus.OK).body(response);
    }
}
