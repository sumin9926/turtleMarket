package turtleMart.order.common;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import turtleMart.global.common.OptionDisplayUtils;
import turtleMart.global.exception.ErrorCode;
import turtleMart.global.exception.NotFoundException;
import turtleMart.order.dto.response.ResolvedProductOption;
import turtleMart.product.entity.Product;
import turtleMart.product.entity.ProductOptionCombination;
import turtleMart.product.repository.ProductOptionCombinationRepository;
import turtleMart.product.repository.ProductOptionValueRepository;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

@Component
@RequiredArgsConstructor
public class ProductOptionResolver {

    private final ProductOptionCombinationRepository combinationRepository;
    private final ProductOptionValueRepository productOptionValueRepository;

    public Map<Long, ResolvedProductOption> resolveProductOptions(List<Long> productOptionIdList){
        List<ProductOptionCombination> combinationList = combinationRepository.findAllByIdIn(productOptionIdList);
        Map<Long, ProductOptionCombination> combinationMap = combinationList.stream()
                .collect(Collectors.toMap(ProductOptionCombination::getId, Function.identity()));
        Map<String, String> uniqueKeyMap = buildOptionInfoMap(combinationList);
        Map<Long, ResolvedProductOption> resolvedProductOptionMap = new HashMap<>();

        for(Long productOptionId: productOptionIdList){
            ProductOptionCombination productOption = combinationMap.get(productOptionId);
            if(null == productOption){
                throw new NotFoundException(ErrorCode.PRODUCT_OPTION_COMBINATION_NOT_FOUND);
            }

            Product product = productOption.getProduct();
            if(null == product){
                throw new NotFoundException(ErrorCode.PRODUCT_NOT_FOUND);
            }

            String optionInfo = uniqueKeyMap.get(productOption.getUniqueKey());

            resolvedProductOptionMap.put(productOptionId,new ResolvedProductOption(product, productOption, optionInfo));
        }
        return resolvedProductOptionMap;
    }

    public Map<String, String> buildOptionInfoMap(List<ProductOptionCombination> productOptionCombinationList) {
        Set<String> uniqueKeySet = productOptionCombinationList.stream()
                .map(ProductOptionCombination::getUniqueKey)
                .collect(Collectors.toSet());
        return OptionDisplayUtils.buildOptionDisplayMap(uniqueKeySet, productOptionValueRepository);
    }
}
