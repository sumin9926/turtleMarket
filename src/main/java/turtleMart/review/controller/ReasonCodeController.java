package turtleMart.review.controller;

import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import turtleMart.review.dto.request.CreateReasonCodeRequest;
import turtleMart.review.dto.request.UpdateReasonCodeRequest;
import turtleMart.review.dto.response.ReasonCodeResponse;
import turtleMart.review.service.ReasonCodeService;

import java.util.List;

@RestController
@RequiredArgsConstructor
public class ReasonCodeController {

    private final ReasonCodeService reasonCodeService;

    @PostMapping("/reason-code")// 관리자 전용
    public ResponseEntity<ReasonCodeResponse> createReasonCode(@RequestBody @Valid CreateReasonCodeRequest request) {
        ReasonCodeResponse reasonCodeResponse = reasonCodeService.createReasonCode(request);
        return ResponseEntity.status(HttpStatus.CREATED).body(reasonCodeResponse);
    }

    @PatchMapping("/reason-code/{reasonCodeId}")// 관리자 전용
    public ResponseEntity<ReasonCodeResponse> updateReasonCode(
            @RequestBody @Valid UpdateReasonCodeRequest request,
            @PathVariable(name = "reasonCodeId") Long reasonCodeId) {

        ReasonCodeResponse reasonCodeResponse = reasonCodeService.updateReasonCode(reasonCodeId, request);
        return ResponseEntity.status(HttpStatus.CREATED).body(reasonCodeResponse);
    }

    @GetMapping("/reason-code")
    public ResponseEntity<List<ReasonCodeResponse>> readReasonCode() {
        List<ReasonCodeResponse> reasonCodeResponseList = reasonCodeService.readAll();
        return ResponseEntity.status(HttpStatus.OK).body(reasonCodeResponseList);
    }

    @DeleteMapping("/reason-code/{reasonCodeId}")//관리자 전용
    public ResponseEntity<Void> deleteReasonCode(@PathVariable(name = "reasonCodeId") Long reasonCodeId) {
        reasonCodeService.delete(reasonCodeId);
        return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
    }


}
