package turtleMart.product.service;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import turtleMart.global.exception.ErrorCode;
import turtleMart.global.exception.NotFoundException;
import turtleMart.product.dto.response.DuplicateList;
import turtleMart.product.dto.request.ProductOptionCombinationRequest;
import turtleMart.product.dto.response.ProductOptionCombinationResponse;
import turtleMart.product.dto.response.ProductOptionCombinationResponseCreate;
import turtleMart.product.entity.Product;
import turtleMart.product.entity.ProductOptionCombination;
import turtleMart.product.entity.ProductOptionMap;
import turtleMart.product.entity.ProductOptionValue;
import turtleMart.product.repository.*;

import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class ProductOptionCombinationService {

    private final ProductRepository productRepository;
    private final ProductOptionMapRepository productOptionMapRepository;
    private final ProductOptionValueRepository productOptionValueRepository;
    private final ProductOptionCombinationRepository productOptionCombinationRepository;
    private final ProductOptionCombinationDslRepository productOptionCombinationDslRepository;

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
}
