package turtleMart.product.dto;

import turtleMart.product.entity.ProductOptionGroup;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;

public record ProductOptionGroupResponseUpdate(
        Long id,
        String name,
        LocalDateTime createdAt,
        LocalDateTime updateAt,
        List<ProductOptionValueResponse> productOptionValueResponseList,
        Map<String, String> passList
) {
    public static ProductOptionGroupResponseUpdate of(ProductOptionGroup productOptionGroup, Map<String, String> passList) {
        return new ProductOptionGroupResponseUpdate(
                productOptionGroup.getId(),
                productOptionGroup.getName(),
                productOptionGroup.getCreatedAt(),
                productOptionGroup.getUpdatedAt(),
                productOptionGroup.getProductOptionValueList().stream().map(ProductOptionValueResponse::from).toList(),
                passList
        );
    }
}
