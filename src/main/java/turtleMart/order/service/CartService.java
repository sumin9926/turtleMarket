package turtleMart.order.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;
import turtleMart.global.common.OptionDisplayUtils;
import turtleMart.global.exception.ErrorCode;
import turtleMart.global.exception.NotFoundException;
import turtleMart.member.repository.MemberRepository;
import turtleMart.order.dto.request.AddCartItemRequest;
import turtleMart.order.dto.request.CartItemQuantityRequest;
import turtleMart.order.dto.response.AddCartItemResponse;
import turtleMart.order.dto.response.CartItemResponse;
import turtleMart.product.entity.Product;
import turtleMart.product.entity.ProductOptionCombination;
import turtleMart.product.repository.ProductOptionCombinationRepository;
import turtleMart.product.repository.ProductOptionValueRepository;
import turtleMart.product.repository.ProductRepository;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class CartService {

    private final MemberRepository memberRepository;
    private final ProductRepository productRepository;
    private final ProductOptionCombinationRepository combinationRepository;
    private final ProductOptionValueRepository productOptionValueRepository;
    private final RedisTemplate<String, String> redisTemplate;
    private final ObjectMapper objectMapper;

    public AddCartItemResponse addItemsToCart(Long memberId, AddCartItemRequest request) throws JsonProcessingException {

        if (!memberRepository.existsById(memberId)) {
            throw new NotFoundException(ErrorCode. MEMBER_NOT_FOUND);
        }

        if (!combinationRepository.existsById(request.productOptionId())) {
            throw new NotFoundException(ErrorCode.PRODUCT_OPTION_COMBINATION_NOT_FOUND);
        }

        String key = "cart:" + memberId;

        //key에 있는 모든 상품을 가져와서, productOptionId 기준으로 비교 (장바구니에 상품이 20개 이상 담길 경우 productId → cartItemId 인덱스 Redis에 따로 두는 것 고려)
        Map<Object, Object> cartEntries = redisTemplate.opsForHash().entries(key);

        for (Map.Entry<Object, Object> entry : cartEntries.entrySet()) {
            String cartItemJson = entry.getValue().toString();
            AddCartItemResponse addCartItemResponse = objectMapper.readValue(cartItemJson, AddCartItemResponse.class);

            if (request.productOptionId().equals(addCartItemResponse.productOptionId())) { //동일 상품 존재시 수량만 업데이트
                Integer updatedQuantity = addCartItemResponse.quantity() + request.quantity();

                return updateCartItemQuantity(memberId, new CartItemQuantityRequest(updatedQuantity), addCartItemResponse.cartItemId());
            }
        }

        //장바구니에 존재하지 않는 상품은 새로 Id를 생성해서 장바구니에 담는다.
        Long cartItemId = UUID.randomUUID().getMostSignificantBits() & Long.MAX_VALUE;

        AddCartItemResponse addCartItemResponse = new AddCartItemResponse(cartItemId, request.productOptionId(), request.quantity(), true);

        String cartItemJson = objectMapper.writeValueAsString(addCartItemResponse);
        redisTemplate.opsForHash().put(key, String.valueOf(cartItemId), cartItemJson);

        return addCartItemResponse;
    }

    public List<CartItemResponse> getItemsFromCart(long memberId) {
        if (!memberRepository.existsById(memberId)) {
            throw new NotFoundException(ErrorCode. MEMBER_NOT_FOUND);
        }

        String key = "cart:" + memberId;
        List<Object> cartJsonList = redisTemplate.opsForHash().values(key); //key에 해당하는 모든 value 가져오기

        List<CartItemResponse> cartItemResponseList = new ArrayList<>();

        for (Object cartJson : cartJsonList) { // JSON 에서 CartItemResponse 로 역직렬화
            try {
                AddCartItemResponse addCartItemResponse = objectMapper.readValue(cartJson.toString(), AddCartItemResponse.class);

                /*TODO 중복로직 수정 예정*/
                ProductOptionCombination productOption = combinationRepository.findById(addCartItemResponse.productOptionId())
                        .orElseThrow(() -> new NotFoundException(ErrorCode.PRODUCT_OPTION_COMBINATION_NOT_FOUND)); //가격 데이터 정합성을 고려해서 DB 직접 조회(따로 캐싱 해두는게 나을 것 같다.)

                Product product = productOption.getProduct();

                if(null==product){
                    throw new NotFoundException(ErrorCode.PRODUCT_NOT_FOUND);
                }

                String optionInfo = OptionDisplayUtils.buildOptionDisplay(productOption.getUniqueKey(), productOptionValueRepository);

                CartItemResponse cartItemResponse = new CartItemResponse(
                        addCartItemResponse.cartItemId(), product.getId(), productOption.getId(), product.getName(),
                        optionInfo, productOption.getPrice(), addCartItemResponse.quantity(), addCartItemResponse.isChecked()
                );
                cartItemResponseList.add(cartItemResponse);
            } catch (JsonProcessingException e) {
                throw new RuntimeException("Redis 장바구니 불러오기 실패", e);
            }
        }

        return cartItemResponseList;
    }

    public AddCartItemResponse updateCartItemQuantity(long memberId, CartItemQuantityRequest request, Long cartItemId) {
        if (!memberRepository.existsById(memberId)) {
            throw new RuntimeException("존재하지 않는 회원입니다.");//TODO 커스텀 예외처리
        }

        String key = "cart:" + memberId;

        Object cartItemJson = redisTemplate.opsForHash().get(key, String.valueOf(cartItemId));
        if (null == cartItemJson) {
            throw new RuntimeException("장바구니에 찾으려는 상품이 존재하지 않습니다."); // TODO 커스텀 예외로 변경하기
        }

        try {
            AddCartItemResponse cartItem = objectMapper.readValue(cartItemJson.toString(), AddCartItemResponse.class);

            AddCartItemResponse updatedCartItem = new AddCartItemResponse(
                    cartItem.cartItemId(), cartItem.productOptionId(), request.quantity(), cartItem.isChecked()
            );

            String updateJson = objectMapper.writeValueAsString(updatedCartItem);
            redisTemplate.opsForHash().put(key, String.valueOf(cartItemId), updateJson);

            return updatedCartItem;

        } catch (JsonProcessingException e) {
            throw new RuntimeException("Redis 장바구니 상품 수량 변경 실패", e);
        }
    }

    public void deleteCartItem(long memberId, Long cartItemId) {
        if (!memberRepository.existsById(memberId)) {
            throw new NotFoundException(ErrorCode.MEMBER_NOT_FOUND);
        }

        String key = "cart:" + memberId;

        Boolean cartItemExist = redisTemplate.opsForHash().hasKey(key, String.valueOf(cartItemId));
        if (Boolean.FALSE.equals(cartItemExist)) {
            throw new NotFoundException(ErrorCode.PRODUCT_NOT_IN_CART);
        }

        redisTemplate.opsForHash().delete(key, String.valueOf(cartItemId));
    }

    public void deleteAllCartItem(long memberId) {
        if (!memberRepository.existsById(memberId)) {
            throw new NotFoundException(ErrorCode.MEMBER_NOT_FOUND);
        }

        String key = "cart:" + memberId;

        redisTemplate.delete(key);
    }
}
