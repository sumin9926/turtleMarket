package turtleMart.order.dto.response;

import java.util.List;

public record MemberOrderListResponse(
        Long memberId,
        List<OrderSimpleResponse> orderedList
) {
    public static MemberOrderListResponse from(Long memberId, List<OrderSimpleResponse> orderedList) {
        return new MemberOrderListResponse(memberId, orderedList);
    }
}
