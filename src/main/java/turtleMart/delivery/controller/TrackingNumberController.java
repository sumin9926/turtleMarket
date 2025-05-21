package turtleMart.delivery.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import turtleMart.delivery.dto.reqeust.CreateTrackingNumberRequest;
import turtleMart.delivery.service.TrackingNumberService;

@RequestMapping("/api/tracking-numbers")
@RestController
@RequiredArgsConstructor
public class TrackingNumberController {

    private final TrackingNumberService trackingNumberService;

    @PostMapping
    public ResponseEntity<?> createTrackingNumber(@RequestBody CreateTrackingNumberRequest request) {
        try {
            String trackingNumber = trackingNumberService.createTrackingNumber(request);

            return ResponseEntity.status(HttpStatus.CREATED).body(trackingNumber);
        } catch (DataIntegrityViolationException e) {
            return ResponseEntity.status(HttpStatus.CONFLICT).body("송장 번호 중복으로 인해 생성에 실패했습니다.");
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body("서버 오류로 인해 송장 번호를 생성하지 못했습니다.");
        }
    }
}
