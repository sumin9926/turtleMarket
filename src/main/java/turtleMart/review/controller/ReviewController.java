package turtleMart.review.controller;

import com.fasterxml.jackson.core.JsonProcessingException;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import turtleMart.review.dto.request.CreateReviewRequest;
import turtleMart.review.dto.request.UpdateReviewRequest;
import turtleMart.review.dto.response.ReviewResponse;
import turtleMart.review.service.ReviewService;

@RestController
@RequiredArgsConstructor
public class ReviewController {

    private final ReviewService reviewService;

    @PostMapping("/products/{productId}/reviews")
    public ResponseEntity<ReviewResponse> createReview(//@RequestAttribute("memberId") Long memberId,
                                                       @PathVariable(name = "productId") Long productId,
                                                       @RequestBody @Valid CreateReviewRequest request)throws JsonProcessingException {
        ReviewResponse reviewResponse = reviewService.createReview(1L, productId, request);
        return ResponseEntity.status(HttpStatus.CREATED).body(reviewResponse);
    }

    @PatchMapping("/reviews/{reviewId}")
    public ResponseEntity<ReviewResponse> updateReview(//@RequestAttribute("memberId") Long memberId,
                                                       @PathVariable(name = "reviewId") Long reviewId,
                                                       @RequestBody UpdateReviewRequest request)throws JsonProcessingException{
        ReviewResponse reviewResponse = reviewService.updateReview(1L, reviewId, request);
        return ResponseEntity.status(HttpStatus.OK).body(reviewResponse);
    }


}
