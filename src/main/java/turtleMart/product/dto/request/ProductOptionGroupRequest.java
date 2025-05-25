package turtleMart.product.dto.request;

import java.util.List;

public record ProductOptionGroupRequest(
        String name,
        List<String> optionNameList
) {
}
