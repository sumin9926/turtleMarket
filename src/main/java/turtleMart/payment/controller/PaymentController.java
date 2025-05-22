package turtleMart.payment.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import turtleMart.payment.dto.response.PaymentResponse;
import turtleMart.payment.service.PaymentService;

import java.util.List;

@RestController
@RequiredArgsConstructor
@RequestMapping("/payments")
public class PaymentController {

    private final PaymentService paymentService;

    @GetMapping("/mine-all")
    public ResponseEntity<List<PaymentResponse>> getAllMyPayments(
            @RequestParam(name = "member") Long memberId) {
        List<PaymentResponse> responses = paymentService.getMyAllPayments(memberId);
        return ResponseEntity.status(HttpStatus.OK).body(responses);
    }

    @GetMapping("/{paymentId}/mine-one")
    public ResponseEntity<PaymentResponse> getMyPayment(
            @PathVariable Long paymentId,
            @RequestParam(name = "member") Long memberId) {
        PaymentResponse response = paymentService.getMyPayment(paymentId, memberId);
        return ResponseEntity.status(HttpStatus.OK).body(response);
    }

    /**
     * 관리자용 조회 메서드.<br>
     * memberId를 입력한 경우 특정 사용자의 결제내역 조회
     */
    @GetMapping("/all")
    public ResponseEntity<List<PaymentResponse>> getAllPayments(
            @RequestParam(name = "member", required = false) Long memberId) {
        List<PaymentResponse> responses = paymentService.getAllPayments(memberId);
        return ResponseEntity.status(HttpStatus.OK).body(responses);
    }
}
