package turtleMart.member.controller;

import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import turtleMart.member.dto.request.SellerRegisterRequest;
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
}
