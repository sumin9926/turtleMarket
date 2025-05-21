package turtleMart.review.dto.response;

import turtleMart.review.entity.ReasonCode;

public record ReasonCodeResponse(Long id, String reason) {

    public static ReasonCodeResponse from(ReasonCode reasonCode){
        return new ReasonCodeResponse(reasonCode.getId(), reasonCode.getReason());
    }
}
