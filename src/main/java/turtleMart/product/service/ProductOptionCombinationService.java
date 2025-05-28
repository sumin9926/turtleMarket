package turtleMart.product.service;

import lombok.RequiredArgsConstructor;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import turtleMart.global.exception.*;
import turtleMart.global.utill.JsonHelper;
import turtleMart.order.entity.OrderItem;
import turtleMart.order.repository.OrderItemRepository;
import turtleMart.product.dto.ProductOptionCombinationPriceDto;
import turtleMart.product.dto.request.ProductOptionCombinationRequest;
import turtleMart.product.dto.response.DuplicateList;
import turtleMart.product.dto.response.ProductOptionCombinationResponse;
import turtleMart.product.dto.response.ProductOptionCombinationResponseCreate;
import turtleMart.product.entity.Product;
import turtleMart.product.entity.ProductOptionCombination;
import turtleMart.product.entity.ProductOptionMap;
import turtleMart.product.entity.ProductOptionValue;
import turtleMart.product.repository.*;

import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.TreeSet;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class ProductOptionCombinationService {

    private final ProductRepository productRepository;
    private final ProductOptionMapRepository productOptionMapRepository;
    private final ProductOptionValueRepository productOptionValueRepository;
    private final ProductOptionCombinationRepository productOptionCombinationRepository;
    private final ProductOptionCombinationDslRepository productOptionCombinationDslRepository;
    private final OrderItemRepository orderItemRepository;
    private final KafkaTemplate<String, String> kafkaTemplate;
    private final RedisTemplate<String, Boolean> redisTemplate;

    @Transactional
    public ProductOptionCombinationResponseCreate createProductOptionCombination(List<ProductOptionCombinationRequest> productOptionCombinationRequest, Long sellerId, Long productId) {
        if (!productRepository.existsById(productId)) {
            throw new NotFoundException(ErrorCode.SELLER_NOT_FOUND);
        }
        List<ProductOptionCombination> productOptionCombinationList = new ArrayList<>();
        List<String> duplicated = new ArrayList<>();
        Product product = productRepository.getReferenceById(productId);
        for (ProductOptionCombinationRequest optionCombinationRequest : productOptionCombinationRequest) {
            TreeSet<Long> valueIdList = new TreeSet<>(optionCombinationRequest.valueIdList());
            String uniqueKey = valueIdList.stream().map(String::valueOf).collect(Collectors.joining("-"));
            if (productOptionCombinationRepository.existsByProductIdAndUniqueKey(productId, uniqueKey)) {
                duplicated.add(uniqueKey);
                continue;
            }
            ProductOptionCombination productOptionCombination =
                    ProductOptionCombination.of(product, optionCombinationRequest.price(), optionCombinationRequest.inventory(),uniqueKey);

            for (Long id : valueIdList) {
                ProductOptionValue productOptionValue = productOptionValueRepository.findById(id).orElseThrow(() -> new NotFoundException(ErrorCode.PRODUCT_OPTION_VALUE_NOT_FOUND));
                ProductOptionMap productOptionMap = ProductOptionMap.of(productOptionCombination, productOptionValue);
                productOptionCombination.addOptionMap(productOptionMap);
            }
            productOptionCombinationList.add(productOptionCombination);
        }
        DuplicateList duplicateList = DuplicateList.from(duplicated);
        productOptionCombinationRepository.saveAll(productOptionCombinationList);
        List<ProductOptionCombinationResponse> productOptionCombinationResponseList = productOptionCombinationList.stream().map(ProductOptionCombinationResponse::from).toList();

        return ProductOptionCombinationResponseCreate.of(productOptionCombinationResponseList, duplicateList);
    }

    public List<ProductOptionCombinationResponse> getAllCombinationByProduct(Long productId) {
        List<ProductOptionCombination> productOptionCombinationList = productOptionCombinationDslRepository.findAllByProductIdWithMapAndValue(productId);
        return productOptionCombinationList.stream().map(ProductOptionCombinationResponse::from).toList();
    }

    public void hardDeleteProductOptionCombination(Long sellerId, Long productOptionCombinationId) {
        ProductOptionCombination productOptionCombination = checkPermission(sellerId, productOptionCombinationId);
        if (orderItemRepository.existsByProductOptionCombinationId(productOptionCombinationId)) {
            throw new BadRequestException(ErrorCode.PRODUCT_OPTION_COMBINATION_ALL_READY_SOLD);
        }
        productOptionCombinationRepository.delete(productOptionCombination);
    }

    private ProductOptionCombination checkPermission(Long sellerId, Long productOptionCombinationId) {
        ProductOptionCombination productOptionCombination = productOptionCombinationDslRepository.findByIdWithProductAndSeller(productOptionCombinationId);
        if (productOptionCombination == null) {
            throw new NotFoundException(ErrorCode.PRODUCT_OPTION_COMBINATION_NOT_FOUND);
        }
        if (!productOptionCombination.getProduct().getSeller().getId().equals(sellerId)) {
            throw new RoleMismatchException(ErrorCode.FORBIDDEN);
        }
        return productOptionCombination;
    }

    public ResponseEntity<Void> updateProductOptionCombinationPrice(Long sellerId, Long productOptionCombinationId, Integer price) {
        checkPermission(sellerId, productOptionCombinationId);
        ProductOptionCombinationPriceDto productOptionCombinationPriceDto = ProductOptionCombinationPriceDto.of(productOptionCombinationId, price);
        String payload = JsonHelper.toJson(productOptionCombinationPriceDto);
        String priceChangeRedisKey = "softLock:priceChange:combination:" + productOptionCombinationId;
        if (redisTemplate.hasKey(priceChangeRedisKey)) {
            return ResponseEntity.status(HttpStatus.CONFLICT).build();
        }
        redisTemplate.opsForValue().set(priceChangeRedisKey,false, Duration.ofMinutes(4));
        kafkaTemplate.send("order_make_topic", productOptionCombinationId.toString(), payload);

        String orderRedisKey = "status:priceChange:combination:" + productOptionCombinationId;
        long timeOut = System.currentTimeMillis() + 300_000;
        while (System.currentTimeMillis() < timeOut) {
            Boolean success = redisTemplate.opsForValue().get(orderRedisKey);
            if (Boolean.TRUE.equals(success)) {
                redisTemplate.delete(orderRedisKey);
                redisTemplate.delete(priceChangeRedisKey);
                return ResponseEntity.status(HttpStatus.OK).build();
            } else if (Boolean.FALSE.equals(success)){
                redisTemplate.delete(orderRedisKey);
                redisTemplate.delete(priceChangeRedisKey);
                return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
            }
            try {
                Thread.sleep(5000);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                throw new CustomRuntimeException(ErrorCode.INTERRUPT);
            }
        }
        throw new CustomRuntimeException(ErrorCode.TIME_OUT);
    }

    @Transactional
    public void decreaseProductOptionCombinationInventory(Long orderId) {
        List<OrderItem> orderItemList = orderItemRepository.findAllByOrderId(orderId);

        for (OrderItem orderItem : orderItemList) {
            Long productOptionCombinationId = orderItem.getProductOptionCombination().getId();
            Integer quantity = orderItem.getQuantity();

            ProductOptionCombination productOptionCombination = productOptionCombinationRepository
                .findByIdWithPessimisticLock(productOptionCombinationId)
                .orElseThrow(() -> new NotFoundException(ErrorCode.PRODUCT_OPTION_COMBINATION_NOT_FOUND));

            if (productOptionCombination.getInventory() < quantity) {
                throw new ConflictException(ErrorCode.PRODUCT_OPTION_COMBINATION_OUT_OF_INVENTORY);
            }

            productOptionCombination.decreaseInventory(quantity);
        }
    }
}
