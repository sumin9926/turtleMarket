package turtleMart.product.service;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import turtleMart.product.dto.request.ProductOptionValueRequest;
import turtleMart.product.entity.ProductOptionGroup;
import turtleMart.product.entity.ProductOptionValue;
import turtleMart.product.repository.ProductOptionValueRepository;

import java.util.*;

@Service
@RequiredArgsConstructor
public class ProductOptionValueService {

    private final ProductOptionValueRepository productOptionValueRepository;

    public void createProductOptionValue(List<String> optionList, ProductOptionGroup productOptionGroup) {
        LinkedHashSet<String> options = new LinkedHashSet<>(optionList);
        for (String name : options) {
            ProductOptionValue productOptionValue = ProductOptionValue.of(name);
            productOptionGroup.addValue(productOptionValue);
        }
    }

    public Map<String, String> updateProductOptionValue(List<ProductOptionValueRequest> productOptionValueRequestList, ProductOptionGroup productOptionGroup) {
        Map<String, String> passList = new LinkedHashMap<>();
        for (ProductOptionValueRequest productOptionValueRequest : productOptionValueRequestList) {
            if (productOptionGroup.duplicate(productOptionValueRequest.name())) {
                passList.put(productOptionValueRequest.name(), "이미 존재하는 옵션");
                continue;
            }
            ProductOptionValue productOptionValue = ProductOptionValue.of(productOptionValueRequest.name());
            productOptionGroup.addValue(productOptionValue);
        }
        return passList;
    }
}
