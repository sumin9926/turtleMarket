package turtleMart.product.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;
import turtleMart.product.dto.request.ProductOptionCombinationRequest;
import turtleMart.product.dto.response.ProductOptionCombinationResponse;
import turtleMart.product.dto.response.ProductOptionCombinationResponseCreate;
import turtleMart.product.service.ProductOptionCombinationService;
import turtleMart.security.AuthUser;

import java.util.List;

@RestController
@RequiredArgsConstructor
public class ProductOptionCombinationController {

    private final ProductOptionCombinationService productOptionCombinationService;

    @PostMapping("/seller/me/products/{productId}/products-option-combination")
    public ResponseEntity<ProductOptionCombinationResponseCreate> createProductOptionCombination(
            @RequestBody List<ProductOptionCombinationRequest> productOptionCombinationRequest,
            @AuthenticationPrincipal AuthUser authUser,
            @PathVariable Long productId
    ) {

        ProductOptionCombinationResponseCreate productOptionCombination =
                productOptionCombinationService.createProductOptionCombination(productOptionCombinationRequest, authUser.memberId(), productId);
        return ResponseEntity.status(HttpStatus.CREATED).body(productOptionCombination);
    }

    @GetMapping("/products/{productId}/products-option-combination")
    public ResponseEntity<List<ProductOptionCombinationResponse>> getAllCombinationByProduct(
            @PathVariable Long productId
    ) {
        List<ProductOptionCombinationResponse> productOptionCombinationResponseList =
                productOptionCombinationService.getAllCombinationByProduct(productId);
        return ResponseEntity.status(HttpStatus.OK).body(productOptionCombinationResponseList);
    }

    @PatchMapping("/seller/me/products-option-combination/{productOptionCombinationId}")
    public ResponseEntity<Void> updateProductOptionCombinationPrice(
            @AuthenticationPrincipal AuthUser authUser,
            @PathVariable Long productOptionCombinationId,
            @RequestParam Integer price
    ) {
       return productOptionCombinationService.updateProductOptionCombinationPrice(authUser.memberId(), productOptionCombinationId, price);
    }

    @PatchMapping("/seller/me/products-option-combination/{productOptionCombinationId}")
    public ResponseEntity<Void> updateProductOptionCombinationInventory(
            @AuthenticationPrincipal AuthUser authUser,
            @PathVariable Long productOptionCombinationId,
            @RequestParam Integer inventory
    ) {
        return productOptionCombinationService.updateProductOptionCombinationInventory(authUser.memberId(), productOptionCombinationId, inventory);
    }

    @DeleteMapping
    public ResponseEntity<Void> softDeleteProductOptionCombination() {

        return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
    }

    @DeleteMapping("/seller/me/products-option-combination/{productOptionCombinationId}")
    public ResponseEntity<Void> hardDeleteProductOptionCombination(
            @AuthenticationPrincipal AuthUser authUser,
            @PathVariable Long productOptionCombinationId
    ) {
        productOptionCombinationService.hardDeleteProductOptionCombination(authUser.memberId(), productOptionCombinationId);
        return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
    }

}
