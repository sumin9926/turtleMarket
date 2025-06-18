package turtleMart.order.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.context.request.async.DeferredResult;
import turtleMart.global.exception.ErrorCode;
import turtleMart.global.exception.ForbiddenException;
import turtleMart.member.entity.Authority;
import turtleMart.order.dto.response.RefundApplyResultResponse;
import turtleMart.order.dto.response.RefundResponse;
import turtleMart.order.service.RefundService;
import turtleMart.order.service.RefundWaiter;
import turtleMart.security.AuthUser;

import java.util.List;

@RestController
@RequestMapping("/orders/refund")
@RequiredArgsConstructor
public class RefundController {

    private final RefundService refundService;
    private final RefundWaiter refundWaiter;

    @PostMapping("/{orderItemId}")
    public ResponseEntity<RefundApplyResultResponse> requestRefund(
            @PathVariable Long orderItemId,
            @AuthenticationPrincipal AuthUser authUser
    ) {
        RefundApplyResultResponse response = refundService.requestRefund(authUser.memberId(), orderItemId);

        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    @GetMapping()
    public ResponseEntity<List<RefundResponse>> getRefundRequestList(
            @AuthenticationPrincipal AuthUser authUser
    ) {
        if(!authUser.hasAuthority(Authority.SELLER)){
            throw new ForbiddenException(ErrorCode.FORBIDDEN);
        }
        List<RefundResponse> responseList = refundService.getRefundRequestList(authUser.memberId());

        return ResponseEntity.status(HttpStatus.OK).body(responseList);
    }

    @PatchMapping("/async/{orderItemId}")
    public DeferredResult<ResponseEntity<Void>> approveRefund(
            @PathVariable Long orderItemId,
            @AuthenticationPrincipal AuthUser authUser
    ){
        if(authUser==null||!authUser.hasAuthority(Authority.SELLER)){
            throw new ForbiddenException(ErrorCode.FORBIDDEN);
        }
        DeferredResult<ResponseEntity<Void>> result = refundWaiter.createWaiter(orderItemId);
        refundService.approveOrderItemRefund(orderItemId, authUser.memberId());

        return result;
    }
}
