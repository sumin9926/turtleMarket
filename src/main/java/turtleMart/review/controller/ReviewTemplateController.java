package turtleMart.review.controller;

import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import turtleMart.review.dto.request.CreateReviewTemplateRequest;
import turtleMart.review.dto.request.UpdateReviewTemplateRequest;
import turtleMart.review.dto.response.ReviewTemplateResponse;
import turtleMart.review.service.ReviewTemplateService;

import java.util.List;

@RestController
@RequiredArgsConstructor
public class ReviewTemplateController {

    private final ReviewTemplateService reviewTemplateService;

    @PostMapping("/review-templates")//관리자 전용
    public ResponseEntity<ReviewTemplateResponse> createReviewTemplate(@RequestBody @Valid CreateReviewTemplateRequest request) {
        ReviewTemplateResponse reviewTemplateResponse = reviewTemplateService.createReviewTemplate(request);
        return ResponseEntity.status(HttpStatus.CREATED).body(reviewTemplateResponse);
    }

    @GetMapping("/review-templates")
    public ResponseEntity<List<ReviewTemplateResponse>> readAll() {// 일반 유저 불가
        List<ReviewTemplateResponse> reviewTemplateResponseList = reviewTemplateService.readAllReviewTemplate();
        return ResponseEntity.status(HttpStatus.OK).body(reviewTemplateResponseList);
    }

    @GetMapping("/products/{productId}/review-templates")
    public ResponseEntity<List<ReviewTemplateResponse>> readByProductId(@PathVariable(name = "productId") Long productId) {
        List<ReviewTemplateResponse> reviewTemplateResponseList = reviewTemplateService.readByProductId(productId);
        return ResponseEntity.status(HttpStatus.OK).body(reviewTemplateResponseList);
    }

    @PatchMapping("/review-templates/{reviewTemplateId}")//관리자 전용
    public ResponseEntity<ReviewTemplateResponse> updateReviewTemplate(
            @RequestBody UpdateReviewTemplateRequest request,
            @PathVariable(name = "reviewTemplateId") Long reviewTemplateId) {

        ReviewTemplateResponse reviewTemplateResponse = reviewTemplateService.updateReviewTemplate(request, reviewTemplateId);
        return ResponseEntity.status(HttpStatus.CREATED).body(reviewTemplateResponse);
    }

    @DeleteMapping("/review-templates/{reviewTemplateId}")//관리자 전용
    public ResponseEntity<Void> deleteReviewTemplate(@PathVariable(name = "reviewTemplateId") Long reviewTemplateId) {
        reviewTemplateService.deleteReviewTemplate(reviewTemplateId);
        return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
    }

}
