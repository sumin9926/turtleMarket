package turtleMart.product.dto;

import java.util.List;

public record ProductOptionGroupRequest(
        String name,
        List<String> optionNameList
) {
}
