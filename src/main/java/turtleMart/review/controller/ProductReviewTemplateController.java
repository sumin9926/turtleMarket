package turtleMart.review.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import turtleMart.review.dto.request.CreateProductReviewTemplateRequest;
import turtleMart.review.service.ProductReviewTemplateService;

@RestController
@RequiredArgsConstructor// 사장 전용
public class ProductReviewTemplateController {

    private final ProductReviewTemplateService productReviewTemplateService;

    @PostMapping("/products/{productId}/product-review-template")
    public ResponseEntity<Void> createProductReviewTemplate(
            @PathVariable(name = "productId") Long productId,
            @RequestBody CreateProductReviewTemplateRequest request
    ) {
        productReviewTemplateService.createProductReviewTemplate(productId, request);
        return ResponseEntity.status(HttpStatus.CREATED).build();
    }

    @DeleteMapping("/product-review-template/{productReviewTemplateId}")
    public ResponseEntity<Void> deleteProductReviewTemplate(
            //@RequestAttribute("memberId") Long memberId,
            @PathVariable(name = "productReviewTemplateId") Long productReviewTemplateId) {

        productReviewTemplateService.deleteProductReviewTemplate(1L, productReviewTemplateId);
        return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
    }
}
