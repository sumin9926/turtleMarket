package turtleMart.order.controller;

import com.fasterxml.jackson.core.JsonProcessingException;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;
import turtleMart.order.dto.request.AddCartItemRequest;
import turtleMart.order.dto.request.CartItemQuantityRequest;
import turtleMart.order.dto.response.AddCartItemResponse;
import turtleMart.order.dto.response.CartItemResponse;
import turtleMart.order.service.CartService;
import turtleMart.security.AuthUser;

import java.util.List;

@RestController
@RequestMapping("/carts")
@RequiredArgsConstructor
public class CartController {

    private final CartService cartService;

    @PostMapping()
    public ResponseEntity<AddCartItemResponse> addCartItem(
            @Valid @RequestBody AddCartItemRequest request,
            @AuthenticationPrincipal AuthUser authUser
    ) throws JsonProcessingException {
        AddCartItemResponse response = cartService.addItemsToCart(authUser.memberId(), request);

        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    @GetMapping("/my")
    public ResponseEntity<List<CartItemResponse>> getMyCartItem(
            @AuthenticationPrincipal AuthUser authUser
    ) {
        List<CartItemResponse> responseList = cartService.getItemsFromCart(authUser.memberId());

        return ResponseEntity.status(HttpStatus.OK).body(responseList);
    }

    @PatchMapping("/cart-items/{cartItemId}")
    public ResponseEntity<AddCartItemResponse> updateCartItemQuantity(
            @AuthenticationPrincipal AuthUser authUser,
            @Valid @RequestBody CartItemQuantityRequest request,
            @PathVariable Long cartItemId
    ) {
        AddCartItemResponse response = cartService.updateCartItemQuantity(authUser.memberId(), request, cartItemId);

        return ResponseEntity.status(HttpStatus.OK).body(response);
    }

    @DeleteMapping("/cart-items/{cartItemId}")
    public ResponseEntity<Void> deleteCartItemById(
            @AuthenticationPrincipal AuthUser authUser,
            @PathVariable Long cartItemId
    ) {
        cartService.deleteCartItem(authUser.memberId(), cartItemId);

        return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
    }

    @DeleteMapping()
    public ResponseEntity<Void> clearCart(
            @AuthenticationPrincipal AuthUser authUser
    ) {
        cartService.deleteAllCartItem(authUser.memberId());

        return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
    }
}
