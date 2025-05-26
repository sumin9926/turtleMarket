package turtleMart.product.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;
import turtleMart.product.dto.request.ApproveOptionRequest;
import turtleMart.product.dto.request.RequestOptionGroupRequest;
import turtleMart.product.dto.response.ProductOptionGroupResponse;
import turtleMart.product.dto.response.RequestOptionGroupResponse;
import turtleMart.product.service.RequestOptionGroupService;
import turtleMart.security.AuthUser;

import java.util.List;

@RestController
@RequiredArgsConstructor
public class RequestOptionGroupController {

    private final RequestOptionGroupService requestOptionGroupService;

    @PostMapping("/seller/me/request-option-group")
    public ResponseEntity<RequestOptionGroupResponse> createRequestOptionGroup(
            @AuthenticationPrincipal AuthUser authUser,
            @RequestBody RequestOptionGroupRequest requestOptionGroupRequest
    ) {
        RequestOptionGroupResponse requestOptionGroupResponse = requestOptionGroupService.createRequestOptionGroup(authUser.memberId(), requestOptionGroupRequest);
        return ResponseEntity.status(HttpStatus.CREATED).body(requestOptionGroupResponse);
    }

    @GetMapping("/seller/me/request-option-group")
    public ResponseEntity<List<RequestOptionGroupResponse>> getAllRequestOptionGroupBySeller(
            @AuthenticationPrincipal AuthUser authUser
    ) {
        List<RequestOptionGroupResponse> requestOptionGroupResponseList = requestOptionGroupService.getAllRequestOptionGroupBySeller(authUser.memberId());
        return ResponseEntity.status(HttpStatus.OK).body(requestOptionGroupResponseList);
    }

    @GetMapping("/request-option-group/not-yet")
    public ResponseEntity<List<RequestOptionGroupResponse>> getAllRequestOptionGroupWithNotYet() {
        List<RequestOptionGroupResponse> requestOptionGroupResponseList = requestOptionGroupService.getAllRequestOptionGroupWithNotYet();
        return ResponseEntity.status(HttpStatus.OK).body(requestOptionGroupResponseList);
    }

    @GetMapping("/request-option-group/all-ready")
    public ResponseEntity<List<RequestOptionGroupResponse>> getAllRequestOptionGroupWithAllReady() {
        List<RequestOptionGroupResponse> requestOptionGroupResponseList = requestOptionGroupService.getAllRequestOptionGroupWithAllReady();
        return ResponseEntity.status(HttpStatus.OK).body(requestOptionGroupResponseList);
    }

    @PatchMapping("/request-option-group/approve")
    public ResponseEntity<ProductOptionGroupResponse> approveRequestOptionGroup(
            @AuthenticationPrincipal AuthUser authUser,
            @RequestBody ApproveOptionRequest approveOptionRequest
    ) {
        ProductOptionGroupResponse productOptionGroupResponse = requestOptionGroupService.approveRequestOptionGroup(authUser, approveOptionRequest);
        return ResponseEntity.status(HttpStatus.CREATED).body(productOptionGroupResponse);
    }

    @DeleteMapping("/seller/me/request-option-group/{requestOptionGroupId}")
    public ResponseEntity<Void> deleteRequestOptionGroup(
            @AuthenticationPrincipal AuthUser authUser,
            @PathVariable Long requestOptionGroupId
    ) {
        requestOptionGroupService.deleteRequestOptionGroup(authUser.memberId(), requestOptionGroupId);
        return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
    }
}
