package turtleMart.product.service;

import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import turtleMart.global.exception.ErrorCode;
import turtleMart.global.exception.NotFoundException;
import turtleMart.product.dto.ProductOptionCombinationRequest;
import turtleMart.product.dto.ProductOptionCombinationResponse;
import turtleMart.product.entity.Product;
import turtleMart.product.entity.ProductOptionCombination;
import turtleMart.product.entity.ProductOptionMap;
import turtleMart.product.entity.ProductOptionValue;
import turtleMart.product.repository.*;

import java.awt.print.Pageable;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

@Service
@RequiredArgsConstructor
public class ProductOptionCombinationService {

    private final ProductRepository productRepository;
    private final ProductOptionMapRepository productOptionMapRepository;
    private final ProductOptionValueRepository productOptionValueRepository;
    private final ProductOptionCombinationRepository productOptionCombinationRepository;
    private final ProductOptionCombinationDslRepository productOptionCombinationDslRepository;

    @Transactional
    public List<ProductOptionCombinationResponse> createProductOptionCombination(List<ProductOptionCombinationRequest> productOptionCombinationRequest, Long sellerId, Long productId) {
        if (!productRepository.existsById(productId)) {
            throw new NotFoundException(ErrorCode.SELLER_NOT_FOUND);
        }
        List<ProductOptionCombination> productOptionCombinationList = new ArrayList<>();
        Product product = productRepository.getReferenceById(productId);
        for (ProductOptionCombinationRequest optionCombinationRequest : productOptionCombinationRequest) {
            ProductOptionCombination productOptionCombination =
                    ProductOptionCombination.of(product, optionCombinationRequest.price(), optionCombinationRequest.inventory());
            LinkedHashSet<Long> valueIdList = new LinkedHashSet<>(optionCombinationRequest.valueIdList());
            //중복 조합 검증로직 필요
            for (Long id : valueIdList) {
                ProductOptionValue productOptionValue = productOptionValueRepository.findById(id).orElseThrow(() -> new NotFoundException(ErrorCode.PRODUCT_OPTION_VALUE_NOT_FOUND));
                ProductOptionMap productOptionMap = ProductOptionMap.of(productOptionCombination, productOptionValue);
                productOptionCombination.addOptionMap(productOptionMap);
            }
            productOptionCombinationList.add(productOptionCombination);
        }
        productOptionCombinationRepository.saveAll(productOptionCombinationList);
        return productOptionCombinationList.stream().map(ProductOptionCombinationResponse::from).toList();
    }

    public List<ProductOptionCombinationResponse> getAllCombinationByProduct(Long productId) {
        List<ProductOptionCombination> productOptionCombinationList = productOptionCombinationDslRepository.findAllByProductIdWithMapAndValue(productId);
        return productOptionCombinationList.stream().map(ProductOptionCombinationResponse::from).toList();
    }
}
