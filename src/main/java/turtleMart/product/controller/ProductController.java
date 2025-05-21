package turtleMart.product.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import turtleMart.product.dto.ProductRequest;
import turtleMart.product.dto.ProductResponse;
import turtleMart.product.dto.ProductResponseForSeller;
import turtleMart.product.service.ProductService;

import java.util.List;

@RestController
@RequiredArgsConstructor
public class ProductController {

    private final ProductService productService;

    @PostMapping("/sellers/{sellerId}/products")
    public ResponseEntity<ProductResponse> createProduct(
            @RequestBody ProductRequest productRequest,
            @PathVariable Long sellerId
    ) {
        ProductResponse productResponse = productService.createProduct(productRequest, sellerId);
        return ResponseEntity.status(HttpStatus.CREATED).body(productResponse);
    }

    @GetMapping("/products/{productId}")
    public ResponseEntity<ProductResponse> getProduct(@PathVariable Long productId) {
        ProductResponse productResponse = productService.getProduct(productId);
        return ResponseEntity.status(HttpStatus.OK).body(productResponse);
    }

    @GetMapping("/sellers/{sellerId}/products")
    public ResponseEntity<List<ProductResponseForSeller>> getProductBySellerId(@PathVariable Long sellerId) {
        List<ProductResponseForSeller> productResponseList = productService.getProductBySellerId(sellerId);
        return ResponseEntity.status(HttpStatus.OK).body(productResponseList);
    }

    @PatchMapping("/sellers/{sellerId}/products/{productId}")
    public ResponseEntity<ProductResponse> updateProduct(
            @RequestBody ProductRequest productRequest,
            @PathVariable Long sellerId,
            @PathVariable Long productId
    ) {
        ProductResponse productResponse = productService.updateProduct(productRequest, sellerId, productId);
        return ResponseEntity.status(HttpStatus.OK).body(productResponse);
    }

    @DeleteMapping("/sellers/{sellerId}/products/{productId}")
    public ResponseEntity<Void> deleteProduct(
            @PathVariable Long productId,
            @PathVariable Long sellerId
    ) {
        productService.deleteProduct(productId, sellerId);
        return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
    }
}
