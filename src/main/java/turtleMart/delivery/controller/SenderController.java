package turtleMart.delivery.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import turtleMart.delivery.dto.reqeust.CreateSenderRequest;
import turtleMart.delivery.dto.reqeust.UpdateSenderRequest;
import turtleMart.delivery.dto.response.SenderResponse;
import turtleMart.delivery.dto.response.UpdateSenderResponse;
import turtleMart.delivery.service.SenderService;

import java.util.List;

@RequestMapping("/api/senders")
@RestController
@RequiredArgsConstructor
public class SenderController {

    private final SenderService senderService;

    @PostMapping
    public ResponseEntity<SenderResponse> createSender(@RequestBody CreateSenderRequest request) {
        SenderResponse senderResponse = senderService.createSender(request);

        return ResponseEntity.status(HttpStatus.CREATED).body(senderResponse);
    }

    @GetMapping
    public ResponseEntity<List<SenderResponse>> readAllSenders() {
        List<SenderResponse> senderResponseList = senderService.readAllSenders();

        return ResponseEntity.status(HttpStatus.OK).body(senderResponseList);
    }

    @GetMapping("/{senderId}")
    public ResponseEntity<SenderResponse> readSender(@PathVariable(name = "senderId") Long senderId) {
        SenderResponse senderResponse = senderService.readSender(senderId);

        return ResponseEntity.status(HttpStatus.OK).body(senderResponse);
    }

    @PatchMapping("/{senderId}")
    public ResponseEntity<UpdateSenderResponse> updateSender(
        @RequestBody UpdateSenderRequest request,
        @PathVariable(name = "senderId") Long senderId
    ) {
        UpdateSenderResponse updateSenderResponse = senderService.updateSender(request, senderId);

        return ResponseEntity.status(HttpStatus.OK).body(updateSenderResponse);
    }

    @DeleteMapping("/{senderId}")
    public ResponseEntity<Void> deleteSender(@PathVariable(name = "senderId") Long senderId) {
        senderService.deleteSender(senderId);

        return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
    }
}
