package turtleMart.order.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;
import turtleMart.order.dto.request.AddCartItemRequest;
import turtleMart.order.dto.request.CartItemQuantityRequest;
import turtleMart.order.dto.response.AddCartItemResponse;
import turtleMart.order.dto.response.CartItemResponse;
import turtleMart.product.entity.Product;
import turtleMart.product.repository.ProductRepository;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class CartService {

    private final ProductRepository productRepository;
    private final RedisTemplate<String, String> redisTemplate;
    private final ObjectMapper objectMapper;

    public AddCartItemResponse addItemsToCart(long memberId, @Valid AddCartItemRequest request) {

        // TODO 회원 조회 및 존재하지 않을 경우 예외처리

        String key = "cart:" + memberId;
        Long cartItemId = UUID.randomUUID().getMostSignificantBits() & Long.MAX_VALUE;

        AddCartItemResponse cartItem = new AddCartItemResponse(
                cartItemId, request.productId(), request.quantity(), true
        );

        try {
            String cartItemJson = objectMapper.writeValueAsString(cartItem);
            redisTemplate.opsForHash().put(key, String.valueOf(cartItemId), cartItemJson);

            return cartItem;

        } catch (JsonProcessingException e) {
            throw new RuntimeException("Redis 장바구니 저장 실패", e);
        }
    }

    public List<CartItemResponse> getItemsFromCart(long memberId) {

        String key = "cart:" + memberId;
        List<Object> cartJsonList = redisTemplate.opsForHash().values(key); //key에 해당하는 모든 value 가져오기

        List<CartItemResponse> cartItemResponseList = new ArrayList<>();

        for (Object cartJson : cartJsonList) { // JSON 에서 CartItemResponse 로 역직렬화
            try {
                AddCartItemResponse addCartItemResponse = objectMapper.readValue(cartJson.toString(), AddCartItemResponse.class);

                Product product = productRepository.findById(addCartItemResponse.productId()).orElseThrow(
                        () -> new RuntimeException("조회되는 상품이 없음") //TODO 커스텀 예외처리
                ); //가격 데이터 정합성을 고려해서 DB 직접 조회(따로 캐싱 해두는게 나을 것 같다.)

                CartItemResponse cartItemResponse = new CartItemResponse(
                        addCartItemResponse.cartItemId(), addCartItemResponse.productId(), product.getName(),
                        product.getPrice(), addCartItemResponse.quantity(), addCartItemResponse.isChecked()
                );
                cartItemResponseList.add(cartItemResponse);
            } catch (JsonProcessingException e) {
                throw new RuntimeException("Redis 장바구니 불러오기 실패", e);
            }
        }

        return cartItemResponseList;
    }

    public AddCartItemResponse updateCartItemQuantity(long memberId, CartItemQuantityRequest request, Long cartItemId) {

        String key = "cart:" + memberId;

        Object cartItemJson = redisTemplate.opsForHash().get(key, String.valueOf(cartItemId));
        if (null == cartItemJson) {
            throw new RuntimeException("장바구니에 찾으려는 상품이 존재하지 않습니다."); // TODO 커스텀 예외로 변경하기
        }

        try {
            AddCartItemResponse cartItem = objectMapper.readValue(cartItemJson.toString(), AddCartItemResponse.class);

            AddCartItemResponse updatedCartItem = new AddCartItemResponse(
                    cartItem.cartItemId(), cartItem.productId(), request.quantity(), cartItem.isChecked()
            );

            String updateJson = objectMapper.writeValueAsString(updatedCartItem);
            redisTemplate.opsForHash().put(key, String.valueOf(cartItemId), updateJson);

            return updatedCartItem;

        } catch (JsonProcessingException e) {
            throw new RuntimeException("Redis 장바구니 상품 수량 변경 실패", e);
        }
    }

    public void deleteCartItem(long memberId, Long cartItemId) {
        String key = "cart:" + memberId;

        Boolean cartItemExist = redisTemplate.opsForHash().hasKey(key, String.valueOf(cartItemId));
        if (Boolean.FALSE.equals(cartItemExist)){
            throw new RuntimeException("장바구니에서 삭제하려는 상품이 존재하지 않음"); //TODO 커스텀 예외처리
        }

        redisTemplate.opsForHash().delete(key, String.valueOf(cartItemId));
    }

    public void deleteAllCartItem(long memberId) {
        String key = "cart:" + memberId;

        redisTemplate.delete(key);
    }
}
