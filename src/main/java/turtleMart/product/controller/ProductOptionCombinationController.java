package turtleMart.product.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.web.bind.annotation.*;
import turtleMart.product.dto.request.ProductOptionCombinationRequest;
import turtleMart.product.dto.response.ProductOptionCombinationResponse;
import turtleMart.product.dto.response.ProductOptionCombinationResponseCreate;
import turtleMart.product.service.ProductOptionCombinationService;

import java.util.List;

@RestController
@RequiredArgsConstructor
public class ProductOptionCombinationController {

    private final ProductOptionCombinationService productOptionCombinationService;

    @PostMapping("/sellers/{sellerId}/products/{productId}/products-option-combination")
    public ResponseEntity<ProductOptionCombinationResponseCreate> createProductOptionCombination(
            @RequestBody List<ProductOptionCombinationRequest> productOptionCombinationRequest,
            @PathVariable Long sellerId,
            @PathVariable Long productId
    ) {

        ProductOptionCombinationResponseCreate productOptionCombination =
                productOptionCombinationService.createProductOptionCombination(productOptionCombinationRequest, sellerId, productId);
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

    @PatchMapping("/sellers/{sellerId}/products-option-combination/{productOptionCombinationId}")
    public ResponseEntity<Void> updateProductOptionCombinationPrice(
            @PathVariable Long sellerId,
            @PathVariable Long productOptionCombinationId,
            @RequestParam Integer price
    ) {
       return productOptionCombinationService.updateProductOptionCombinationPrice(sellerId, productOptionCombinationId, price);
    }

    @PatchMapping("/sellers/{sellerId}/products-option-combination")
    public ResponseEntity<Void> updateProductOptionCombinationInventory() {
        return null;
    }

    @DeleteMapping
    public ResponseEntity<Void> softDeleteProductOptionCombination() {

        return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
    }

    @DeleteMapping("/sellers/{sellerId}/products-option-combination/{productOptionCombinationId}")
    public ResponseEntity<Void> hardDeleteProductOptionCombination(
            @PathVariable Long sellerId,
            @PathVariable Long productOptionCombinationId
    ) {
        productOptionCombinationService.hardDeleteProductOptionCombination(sellerId, productOptionCombinationId);
        return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
    }

}
