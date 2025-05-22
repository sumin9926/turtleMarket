package turtleMart.order.controller;

import com.fasterxml.jackson.core.JsonProcessingException;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import turtleMart.order.dto.request.AddCartItemRequest;
import turtleMart.order.dto.request.CartItemQuantityRequest;
import turtleMart.order.dto.response.AddCartItemResponse;
import turtleMart.order.dto.response.CartItemResponse;
import turtleMart.order.service.CartService;

import java.util.List;

@RestController
@RequestMapping("/carts")
@RequiredArgsConstructor
public class CartController {

    private final CartService cartService;

    /*TODO 전체 메서드에 회원 권한 추가*/

    @PostMapping()
    public ResponseEntity<AddCartItemResponse> addCartItem(
            @Valid @RequestBody AddCartItemRequest request
            /*TODO JWT 통해서 회원 ID 가져오기*/
    ) throws JsonProcessingException {
        AddCartItemResponse response = cartService.addItemsToCart(1L, request);

        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    @GetMapping("/my")
    public ResponseEntity<List<CartItemResponse>> getMyCartItem(
            /*TODO JWT 통해서 회원 ID 가져오기*/
    ) {
        List<CartItemResponse> responseList = cartService.getItemsFromCart(1L);

        return ResponseEntity.status(HttpStatus.OK).body(responseList);
    }

    @PatchMapping("/cart-items/{cartItemId}")
    public ResponseEntity<AddCartItemResponse> updateCartItemQuantity(
            /*TODO JWT 통해서 회원 ID 가져오기*/
            @Valid @RequestBody CartItemQuantityRequest request,
            @PathVariable Long cartItemId
    ) {
        AddCartItemResponse response = cartService.updateCartItemQuantity(1L, request, cartItemId);

        return ResponseEntity.status(HttpStatus.OK).body(response);
    }

    @DeleteMapping("/cart-items/{cartItemId}")
    public ResponseEntity<Void> deleteCartItemById(
            /*TODO JWT 통해서 회원 ID 가져오기*/
            @PathVariable Long cartItemId
    ) {
        cartService.deleteCartItem(1L, cartItemId);

        return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
    }

    @DeleteMapping()
    public ResponseEntity<Void> clearCart(
            /*TODO JWT 통해서 회원 ID 가져오기*/
    ) {
        cartService.deleteAllCartItem(1L);

        return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
    }
}
