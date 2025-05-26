package turtleMart.product.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import turtleMart.product.dto.request.ProductOptionGroupRequest;
import turtleMart.product.dto.request.ProductOptionGroupRequestUpdate;
import turtleMart.product.dto.request.ProductOptionValueRequest;
import turtleMart.product.dto.request.ProductOptionValueUpdateRequest;
import turtleMart.product.dto.response.ProductOptionGroupResponse;
import turtleMart.product.dto.response.ProductOptionGroupResponseUpdate;
import turtleMart.product.service.ProductOptionGroupService;

import java.util.List;

@RestController
@RequiredArgsConstructor
public class ProductOptionGroupController {

    private final ProductOptionGroupService productOptionGroupService;

    @PostMapping("/members/{memberId}/products-option-group")
    public ResponseEntity<ProductOptionGroupResponse> createProductOptionGroup(
            @RequestBody ProductOptionGroupRequest productOptionGroupRequest,
            @PathVariable Long memberId
    ) {
        ProductOptionGroupResponse productOptionGroupResponse = productOptionGroupService.createProductOptionGroup(productOptionGroupRequest, memberId);
        return ResponseEntity.status(HttpStatus.CREATED).body(productOptionGroupResponse);
    }

    @PostMapping("/members/{memberId}/products-option-group/{productOptionGroupId}/proucts-option-value")
    public ResponseEntity<ProductOptionGroupResponseUpdate> createProductOptionValue(
            @RequestBody List<ProductOptionValueRequest> productOptionValueRequest,
            @PathVariable Long memberId,
            @PathVariable Long productOptionGroupId
    ) {
        ProductOptionGroupResponseUpdate productOptionGroupResponse = productOptionGroupService.createProductOptionValue(productOptionValueRequest, memberId, productOptionGroupId);
        return ResponseEntity.status(HttpStatus.CREATED).body(productOptionGroupResponse);

    }

    @GetMapping("/products-option-group")
    public ResponseEntity<Page<ProductOptionGroupResponse>> getAllProductOptionGroup(
            @PageableDefault(page = 0, size = 10, sort = "createdAt", direction = Sort.Direction.DESC)
            Pageable pageable) {
        Page<ProductOptionGroupResponse> productOptionGroupResponse = productOptionGroupService.getAllProductOptionGroup(pageable);
        return ResponseEntity.status(HttpStatus.OK).body(productOptionGroupResponse);
    }

    @PatchMapping("/members/{memberId}/products-option-group/{productOptionGroupId}")
    public ResponseEntity<ProductOptionGroupResponse> updateProductOptionGroup(
            @RequestBody ProductOptionGroupRequestUpdate productOptionGroupRequest,
            @PathVariable Long memberId,
            @PathVariable Long productOptionGroupId
    ) {
        ProductOptionGroupResponse productOptionGroupResponse =
                productOptionGroupService.updateProductOptionGroup(productOptionGroupRequest,memberId,productOptionGroupId);
        return ResponseEntity.status(HttpStatus.OK).body(productOptionGroupResponse);
    }

    @PatchMapping("/members/{memberId}/products-option-group/{productOptionGroupId}/products-option-value")
    public ResponseEntity<ProductOptionGroupResponse> updateProductOptionValue(
            @RequestBody List<ProductOptionValueUpdateRequest> productOptionValueRequest,
            @PathVariable Long memberId,
            @PathVariable Long productOptionGroupId
    ) {
        ProductOptionGroupResponse productOptionGroupResponse =
                productOptionGroupService.updateProductOptionValue(productOptionValueRequest,memberId, productOptionGroupId);
        return ResponseEntity.status(HttpStatus.OK).body(productOptionGroupResponse);
    }

    @DeleteMapping("/members/{memberId}/products-option-group/{productOptionGroupId}")
    public ResponseEntity<Void> deleteProductOptionGroup(
            @PathVariable Long productOptionGroupId,
            @PathVariable Long memberId
            ) {
        productOptionGroupService.deleteProductOptionGroup(productOptionGroupId, memberId);
        return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
    }

    @DeleteMapping("/members/{memberId}/products-option-group/{productOptionGroupId}/products-option-value")
    public ResponseEntity<Void> deleteProductOptionValue(
        @PathVariable Long memberId,
        @PathVariable Long productOptionGroupId,
        @RequestParam List<Long> productOptionValueList
    ) {
        productOptionGroupService.deleteProductOptionValue(productOptionValueList, memberId, productOptionGroupId);
        return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
    }
}
