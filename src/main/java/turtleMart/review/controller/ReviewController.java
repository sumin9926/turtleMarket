package turtleMart.review.controller;

import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;
import turtleMart.global.common.CursorPageResponse;
import turtleMart.review.dto.request.CreateReviewRequest;
import turtleMart.review.dto.request.UpdateReviewRequest;
import turtleMart.review.dto.response.ReviewResponse;
import turtleMart.review.service.ReviewService;
import turtleMart.security.AuthUser;

@Slf4j
@RestController
@RequiredArgsConstructor
public class ReviewController {

    private final ReviewService reviewService;

    @PostMapping("/products/{productId}/reviews")
    public ResponseEntity<ReviewResponse> createReview(
            @AuthenticationPrincipal AuthUser authUser,
            @PathVariable(name = "productId") Long productId,
            @RequestBody @Valid CreateReviewRequest request) {

        ReviewResponse reviewResponse = reviewService.createReview(authUser.memberId(), productId, request);
        return ResponseEntity.status(HttpStatus.CREATED).body(reviewResponse);
    }

    @GetMapping("/reviews/{reviewId}")
    public ResponseEntity<ReviewResponse> readReview(
            @PathVariable(name = "reviewId") Long reviewId) {

        ReviewResponse reviewResponse = reviewService.readReview(reviewId);
        return ResponseEntity.status(HttpStatus.OK).body(reviewResponse);
    }

    @GetMapping("/members/reviews")
    public ResponseEntity<Page<ReviewResponse>> readByMemberId(@AuthenticationPrincipal AuthUser authUser,
                                                               @RequestParam(name = "size", required = false, defaultValue = "10") int size,
                                                               @RequestParam(name = "page", required = false, defaultValue = "1") int page
    ) {
        Pageable pageable = PageRequest.of(page - 1, size);
        Page<ReviewResponse> reviewResponse = reviewService.readByMemberId(authUser.memberId(), pageable);
        return ResponseEntity.status(HttpStatus.OK).body(reviewResponse);
    }

    @GetMapping("/products/{productId}/reviews")
    public ResponseEntity<CursorPageResponse<ReviewResponse>> readByProductIdAndCondition(@PathVariable(name = "productId") Long productId,
                                                                            @RequestParam(name = "keyWord", required = false) String keyWord,
                                                                            @RequestParam(name = "rating", required = false) Integer rating,
                                                                            @RequestParam(name = "size", required = false, defaultValue = "10") Integer size,
                                                                            @RequestParam(name = "cursor", required = false) Long cursor
    ) {
        CursorPageResponse<ReviewResponse> reviewResponse = reviewService.readByProductIdWithSearch(productId, keyWord, rating, size, cursor);
        return ResponseEntity.status(HttpStatus.OK).body(reviewResponse);
    }

    @PatchMapping("/reviews/{reviewId}")
    public ResponseEntity<ReviewResponse> updateReview(
            @AuthenticationPrincipal AuthUser authUser,
            @PathVariable(name = "reviewId") Long reviewId,
            @RequestBody UpdateReviewRequest request) {

        ReviewResponse reviewResponse = reviewService.updateReview(authUser.memberId(), reviewId, request);
        return ResponseEntity.status(HttpStatus.OK).body(reviewResponse);
    }

    @DeleteMapping("/reviews/{reviewId}")
    public ResponseEntity<Void> deleteReview(
            @AuthenticationPrincipal AuthUser authUser,
            @PathVariable(name = "reviewId") Long reviewId) {

        reviewService.deleteReview(authUser.memberId(), reviewId);
        return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
    }
}
