package turtleMart.payment.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import turtleMart.payment.service.EmailService;

@RestController
@RequestMapping("/webhook")
@RequiredArgsConstructor
public class EmailController {

    private final EmailService emailService;

    @PostMapping("/toss-payment")
    public ResponseEntity<Void> handleTossWebhook(@RequestBody String payload) {
        return ResponseEntity.ok().build();
    }
}
