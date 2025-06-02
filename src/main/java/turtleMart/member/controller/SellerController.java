package turtleMart.member.controller;

import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;
import turtleMart.member.dto.request.DeleteSellerRequest;
import turtleMart.member.dto.request.SellerRegisterRequest;
import turtleMart.member.dto.request.UpdateSellerRequest;
import turtleMart.member.dto.response.SellerResponse;
import turtleMart.member.service.SellerService;
import turtleMart.security.AuthUser;

@RestController
@RequestMapping("/members/sellers")
@RequiredArgsConstructor
public class SellerController {
    private final SellerService sellerService;

    @PostMapping("register")
    public ResponseEntity<SellerResponse> registerSeller(
            @AuthenticationPrincipal AuthUser authUser,
            @RequestBody @Valid SellerRegisterRequest request
    ) {
        SellerResponse sellerResponse = sellerService.registerSeller(authUser.memberId(), request);
        return ResponseEntity.status(HttpStatus.CREATED).body(sellerResponse);
    }

    @GetMapping("/{sellerId}")
    public ResponseEntity<SellerResponse> getSeller(
            @PathVariable Long sellerId
    ) {
        SellerResponse seller = sellerService.findSellerInfo(sellerId);
        return ResponseEntity.status(HttpStatus.OK).body(seller);
    }

    @GetMapping("/{sellerId}/me")
    public ResponseEntity<SellerResponse> getMySellerProfile(
            @AuthenticationPrincipal AuthUser authUser,
            @PathVariable Long sellerId
    ) {
        SellerResponse mySellerProfile = sellerService.findMySellerProfile(authUser.memberId(), sellerId);
        return ResponseEntity.status(HttpStatus.OK).body(mySellerProfile);
    }

    @PatchMapping("/{sellerId}/me/modify")
    public ResponseEntity<SellerResponse> updateMySellerProfile(
            @AuthenticationPrincipal AuthUser authUser,
            @PathVariable Long sellerId,
            @RequestBody @Valid UpdateSellerRequest request
    ) {
        SellerResponse sellerResponse = sellerService.updateSellerProfile(authUser.memberId(), sellerId, request);
        return ResponseEntity.status(HttpStatus.OK).body(sellerResponse);
    }

    @DeleteMapping("/{sellerId}/me/close")
    public ResponseEntity<String> withdrawSeller(
            @AuthenticationPrincipal AuthUser authUser,
            @PathVariable Long sellerId,
            @RequestBody @Valid DeleteSellerRequest request
    ) {
        String resultMessage = sellerService.deleteSeller(authUser.memberId(), sellerId, request);
        return ResponseEntity.status(HttpStatus.OK).body(resultMessage);
    }
}
