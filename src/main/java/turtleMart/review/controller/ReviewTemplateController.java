package turtleMart.review.controller;

import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;
import turtleMart.review.dto.request.CreateReviewTemplateRequest;
import turtleMart.review.dto.response.ReviewTemplateResponse;
import turtleMart.review.service.ReviewTemplateService;

@RestController
@RequiredArgsConstructor //관리자 전용
public class ReviewTemplateController {

    private final ReviewTemplateService reviewTemplateService;

    @PostMapping("/review_templates")
    public ResponseEntity<ReviewTemplateResponse> createReviewTemplate(@RequestBody @Valid CreateReviewTemplateRequest request){
        ReviewTemplateResponse reviewTemplateResponse = reviewTemplateService.createReviewTemplate(request);
        return ResponseEntity.status(HttpStatus.CREATED).body(reviewTemplateResponse);
    }
}
