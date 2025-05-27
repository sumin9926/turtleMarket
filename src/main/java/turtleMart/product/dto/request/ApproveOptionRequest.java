package turtleMart.product.dto.request;

import java.util.List;

public record ApproveOptionRequest(
        Long requestOptionGroupId,
        List<Long> requestOptionValueIdList
) {
}
