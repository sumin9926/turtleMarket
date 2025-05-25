package turtleMart.product.dto.request;

import java.util.List;

public record ProductOptionCombinationRequest(
        List<Long> valueIdList,
        Integer price,
        Integer inventory
) {
}
