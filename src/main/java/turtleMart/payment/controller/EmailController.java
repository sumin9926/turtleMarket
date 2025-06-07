package turtleMart.payment.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import turtleMart.payment.dto.request.EmailRequest;
import turtleMart.payment.service.EmailService;

@RestController
@RequestMapping("/emails")
@RequiredArgsConstructor
public class EmailController {

    private final EmailService emailService;

    @PostMapping("/payment-confirmation")
    public ResponseEntity<Void> sendPaymentConfirmationEmail(@RequestBody EmailRequest request) {
        emailService.sendPaymentCompleteEmail(request.to(), request.subject(), request.content());
        return ResponseEntity.ok().build();
    }
}
