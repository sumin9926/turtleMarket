package turtleMart.order.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import turtleMart.order.dto.response.RefundApplyResultResponse;
import turtleMart.order.service.RefundService;

@RestController
@RequestMapping("/orders/refund")
@RequiredArgsConstructor
public class RefundController {

    private final RefundService refundService;

    @PostMapping("/{orderItemId}")
    public ResponseEntity<RefundApplyResultResponse> applyRefund(
            @PathVariable Long orderItemId
            /*TODO 토큰에서 회원 ID 가져오기*/
    ){
        RefundApplyResultResponse response = refundService.applyRefund(1L, orderItemId);

        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }
}
