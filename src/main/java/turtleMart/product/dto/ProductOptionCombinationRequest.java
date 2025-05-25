package turtleMart.product.dto;

import java.util.List;

public record ProductOptionCombinationRequest(
        List<Long> valueIdList,
        Integer price,
        Integer inventory
) {
}
