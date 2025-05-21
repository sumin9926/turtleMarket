package turtleMart.review.controller;

import com.fasterxml.jackson.core.JsonProcessingException;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import turtleMart.review.dto.request.CreateReviewRequest;
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


}
