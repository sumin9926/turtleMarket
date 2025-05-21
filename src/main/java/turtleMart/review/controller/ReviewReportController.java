package turtleMart.review.controller;

import com.fasterxml.jackson.core.JacksonException;
import com.fasterxml.jackson.core.JsonProcessingException;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import turtleMart.review.dto.request.CreateReviewReportRequest;
import turtleMart.review.dto.response.ReviewReportResponse;
import turtleMart.review.service.ReviewReportService;

@RestController
@RequiredArgsConstructor
public class ReviewReportController {

    private final ReviewReportService reviewReportService;

    @PostMapping("/reviews/{reviewId}/review-reports")
    public ResponseEntity<ReviewReportResponse> createReviewReport(//@RequestAttribute("memberId") Long memberId,
                                                                   @PathVariable(name = "reviewId") Long reviewId,
                                                                   @RequestBody @Valid CreateReviewReportRequest request
    ) throws JsonProcessingException {
        ReviewReportResponse reviewReportResponse = reviewReportService.createReviewReport(1L, reviewId, request);
        return ResponseEntity.status(HttpStatus.CREATED).body(reviewReportResponse);
    }
}
