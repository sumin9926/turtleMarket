package turtleMart.payment.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import turtleMart.payment.dto.response.PaymentResponse;
import turtleMart.payment.service.PaymentService;

import java.util.List;

@RestController
@RequiredArgsConstructor
@RequestMapping("/payments")
public class PaymentController {

    private final PaymentService paymentService;

    @GetMapping("/mine-all")
    public ResponseEntity<List<PaymentResponse>> getAllMyPayments(Long memberId) {
        List<PaymentResponse> responses = paymentService.getMyAllPayments(memberId);
        return ResponseEntity.status(HttpStatus.OK).body(responses);
    }

    @GetMapping("/mine-one")
    public ResponseEntity<PaymentResponse> getMyPayment(Long paymentId, Long memberId) {
        PaymentResponse response = paymentService.getMyPayment(paymentId, memberId);
        return ResponseEntity.status(HttpStatus.OK).body(response);
    }

    /**
     * 관리자용 조회 메서드.<br>
     * memberId를 입력한 경우 특정 사용자의 결제내역 조회
     */
    @GetMapping("/all")
    public ResponseEntity<List<PaymentResponse>> getAllPayments(Long memberId) {
        List<PaymentResponse> responses = paymentService.getAllPayments(memberId);
        return ResponseEntity.status(HttpStatus.OK).body(responses);
    }
}
