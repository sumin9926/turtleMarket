package turtleMart.product.service;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
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

    public Map<String, String> updateProductOptionValue(List<String> optionList, ProductOptionGroup productOptionGroup) {
        Map<String, String> passList = new LinkedHashMap<>();
        for (String name : optionList) {
            if (productOptionGroup.duplicate(name)) {
                passList.put(name, "이미 존재하는 옵션");
                continue;
            }
            ProductOptionValue productOptionValue = ProductOptionValue.of(name);
            productOptionGroup.getProductOptionValueList().add(productOptionValue);
        }

        return passList;
    }
}
