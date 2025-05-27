package turtleMart.product.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.PageableDefault;
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
    public ResponseEntity<Page<RequestOptionGroupResponse>> getAllRequestOptionGroupBySeller(
            @AuthenticationPrincipal AuthUser authUser,
            @PageableDefault(page = 0, size = 10, sort = "createdAt", direction = Sort.Direction.DESC)
            Pageable pageable
    ) {
        Page<RequestOptionGroupResponse> requestOptionGroupResponseList = requestOptionGroupService.getAllRequestOptionGroupBySeller(authUser.memberId(),pageable);
        return ResponseEntity.status(HttpStatus.OK).body(requestOptionGroupResponseList);
    }

    @GetMapping("/request-option-group/not-yet")
    public ResponseEntity<Page<RequestOptionGroupResponse>> getAllRequestOptionGroupWithNotYet(
            @PageableDefault(page = 0, size = 10, sort = "createdAt", direction = Sort.Direction.DESC)
            Pageable pageable
    ) {
        Page<RequestOptionGroupResponse> requestOptionGroupResponseList = requestOptionGroupService.getAllRequestOptionGroupWithNotYet(pageable);
        return ResponseEntity.status(HttpStatus.OK).body(requestOptionGroupResponseList);
    }

    @GetMapping("/request-option-group/all-ready")
    public ResponseEntity<Page<RequestOptionGroupResponse>> getAllRequestOptionGroupWithAllReady(
            @PageableDefault(page = 0, size = 10, sort = "createdAt", direction = Sort.Direction.DESC)
            Pageable pageable
    ) {
        Page<RequestOptionGroupResponse> requestOptionGroupResponseList = requestOptionGroupService.getAllRequestOptionGroupWithAllReady(pageable);
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
