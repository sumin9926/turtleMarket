package turtleMart.delivery.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import turtleMart.delivery.dto.reqeust.CreateSenderRequest;
import turtleMart.delivery.dto.response.SenderResponse;
import turtleMart.delivery.service.SenderService;

import java.util.List;

@RequestMapping
@RestController
@RequiredArgsConstructor
public class SenderController {

    private final SenderService senderService;

    @PostMapping("/api/senders")
    public ResponseEntity<SenderResponse> createSender(@RequestBody CreateSenderRequest request) {
        SenderResponse senderResponse = senderService.createSender(request);

        return ResponseEntity.status(HttpStatus.CREATED).body(senderResponse);
    }

    @GetMapping("/api/senders")
    public ResponseEntity<List<SenderResponse>> readAllSenders() {
        List<SenderResponse> senderResponseList = senderService.readAllSenders();

        return ResponseEntity.status(HttpStatus.OK).body(senderResponseList);
    }

    @GetMapping("/api/senders/{senderId}")
    public ResponseEntity<SenderResponse> readSender(@PathVariable(name = "senderId") Long senderId) {
        SenderResponse senderResponse = senderService.readSender(senderId);

        return ResponseEntity.status(HttpStatus.OK).body(senderResponse);
    }
}
