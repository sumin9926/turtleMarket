package turtleMart.product.dto.response;

import turtleMart.product.entity.ProductOptionGroup;

import java.time.LocalDateTime;
import java.util.List;

public record ProductOptionGroupResponse(
        Long id,
        String name,
        LocalDateTime createdAt,
        LocalDateTime updateAt,
        List<ProductOptionValueResponse> productOptionValueResponseList
) {
    public static ProductOptionGroupResponse from(ProductOptionGroup productOptionGroup) {
        return new ProductOptionGroupResponse(
                productOptionGroup.getId(),
                productOptionGroup.getName(),
                productOptionGroup.getCreatedAt(),
                productOptionGroup.getUpdatedAt(),
                productOptionGroup.getProductOptionValueList().stream().map(ProductOptionValueResponse::from).toList()
        );
    }
}
