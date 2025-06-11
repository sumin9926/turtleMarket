package turtleMart.review.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;
import turtleMart.review.dto.request.CreateProductReviewTemplateRequest;
import turtleMart.review.dto.response.ProductReviewTemplateResponse;
import turtleMart.review.service.ProductReviewTemplateService;
import turtleMart.security.AuthUser;

import java.util.List;

@RestController
@RequiredArgsConstructor// 사장 전용
public class ProductReviewTemplateController {

    private final ProductReviewTemplateService productReviewTemplateService;

    @PostMapping("/products/{productId}/product-review-template")
    public ResponseEntity<List<ProductReviewTemplateResponse>> createProductReviewTemplate(
            @PathVariable(name = "productId") Long productId,
            @RequestBody CreateProductReviewTemplateRequest request
    ) {
        List<ProductReviewTemplateResponse> productReviewTemplateResponseList = productReviewTemplateService.createProductReviewTemplate(productId, request);
        return ResponseEntity.status(HttpStatus.CREATED).body(productReviewTemplateResponseList);
    }

//    @PatchMapping("/product-review-template/{productReviewTemplateId}")
//    public ResponseEntity<ProductReviewTemplateResponse> updateProductReviewTemplate(@RequestBody CreateProductReviewTemplateRequest request){
//
//    }

    @DeleteMapping("/product-review-template/{productReviewTemplateId}")
    public ResponseEntity<Void> deleteProductReviewTemplate(
            @AuthenticationPrincipal AuthUser authUser,
            @PathVariable(name = "productReviewTemplateId") Long productReviewTemplateId) {

        productReviewTemplateService.deleteProductReviewTemplate(authUser.memberId(), productReviewTemplateId);
        return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
    }
}
