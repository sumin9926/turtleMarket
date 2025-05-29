package turtleMart.product.service;

import org.springframework.stereotype.Service;
import turtleMart.product.entity.ProductOptionValue;
import turtleMart.product.entity.RequestOptionGroup;
import turtleMart.product.entity.RequestOptionValue;

import java.util.LinkedHashSet;
import java.util.List;

@Service
public class RequestOptionValueService {
    public void createRequestOptionValue(RequestOptionGroup requestOptionGroup, List<String> valueNameList) {
        LinkedHashSet<String> options = new LinkedHashSet<>(valueNameList);
        for (String name : options) {
            RequestOptionValue requestOptionValue = RequestOptionValue.of(name);
            requestOptionGroup.addValue(requestOptionValue);
        }
    }
}
