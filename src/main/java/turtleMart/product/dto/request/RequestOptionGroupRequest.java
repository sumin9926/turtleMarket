package turtleMart.product.dto.request;

import java.util.List;

public record RequestOptionGroupRequest(
        String name,
        List<String> valueNameList
) {
}
