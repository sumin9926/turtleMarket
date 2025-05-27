package turtleMart.product.dto.request;

import turtleMart.product.entity.RequestOptionValueStatus;

import java.util.List;

public record ApproveOptionRequest(
        Long requestOptionGroupId,
        List<Long> requestOptionValueIdList,
        List<RequestOptionValueStatus> requestOptionValueStatusList,
        String rejectionReason
) {
}
