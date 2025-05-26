package turtleMart.product.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.context.request.async.DeferredResult;
import turtleMart.global.component.DeferredResultStore;
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
    private final DeferredResultStore deferredResultStore;
    private final RedisTemplate<String, Object> redisTemplate;

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

    @PatchMapping("/seller/me/products-option-combination/{productOptionCombinationId}/price")
    public DeferredResult<ResponseEntity<?>> updateProductOptionCombinationPrice(
            @AuthenticationPrincipal AuthUser authUser,
            @PathVariable Long productOptionCombinationId,
            @RequestParam Integer price
    ) {
        String operationId = productOptionCombinationService.updateProductOptionCombinationPrice(authUser.memberId(), productOptionCombinationId, price);
        DeferredResult<ResponseEntity<?>> objectDeferredResult = new DeferredResult<>(300_000L);
        deferredResultStore.put(operationId, objectDeferredResult);
        objectDeferredResult.onTimeout(() -> {
            deferredResultStore.remove(operationId);
            String redisKey = "status:" + operationId;
            Boolean success = (Boolean) redisTemplate.opsForValue().get(redisKey);
            if (Boolean.TRUE.equals(success)) {
                objectDeferredResult.setResult(ResponseEntity.ok().build());
            } else if (Boolean.FALSE.equals(success)) {
                objectDeferredResult.setResult(ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build());
            } else {
                objectDeferredResult.setErrorResult(ResponseEntity.status(HttpStatus.REQUEST_TIMEOUT).build());
            }
            //주문생성 소프트락도 여기서 해제
            redisTemplate.delete(redisKey);
            redisTemplate.delete("softLock:priceChange:combination:" + productOptionCombinationId);
        });
        return objectDeferredResult;
    }


    @PatchMapping("/seller/me/products-option-combination/{productOptionCombinationId}/inventory")
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
