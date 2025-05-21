package turtleMart.delivery.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import turtleMart.delivery.dto.reqeust.CreateSenderRequest;
import turtleMart.delivery.dto.response.CreateSenderResponse;
import turtleMart.delivery.service.SenderService;

@RequestMapping
@RestController
@RequiredArgsConstructor
public class SenderController {

    private final SenderService senderService;

    @PostMapping("/api/senders")
    public ResponseEntity<CreateSenderResponse> createSender(@RequestBody CreateSenderRequest request) {
        CreateSenderResponse createSenderResponse = senderService.createSender(request);

        return ResponseEntity.status(HttpStatus.CREATED).body(createSenderResponse);
    }
}
