package turtleMart.product.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import turtleMart.product.dto.ProductOptionCombinationRequest;
import turtleMart.product.dto.ProductOptionCombinationResponse;
import turtleMart.product.service.ProductOptionCombinationService;

import java.awt.print.Pageable;
import java.util.List;

@RestController
@RequiredArgsConstructor
public class ProductOptionCombinationController {

    private final ProductOptionCombinationService productOptionCombinationService;

    @PostMapping("/sellers/{sellerId}/products/{productId}/products-option-combination")
    public ResponseEntity<List<ProductOptionCombinationResponse>> createProductOptionCombination(
            @RequestBody List<ProductOptionCombinationRequest> productOptionCombinationRequest,
            @PathVariable Long sellerId,
            @PathVariable Long productId
    ) {
        List<ProductOptionCombinationResponse> productOptionCombination =
                productOptionCombinationService.createProductOptionCombination(productOptionCombinationRequest, sellerId,productId);
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
}
