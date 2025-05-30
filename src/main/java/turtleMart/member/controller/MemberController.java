package turtleMart.member.controller;

import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;
import turtleMart.member.dto.request.MemberWithdrawRequest;
import turtleMart.member.dto.request.updatePasswordRequest;
import turtleMart.member.dto.request.updateProfileRequest;
import turtleMart.member.dto.response.MemberResponse;
import turtleMart.member.service.MemberService;
import turtleMart.security.AuthUser;

@RestController
@RequestMapping("/members")
@RequiredArgsConstructor
public class MemberController {
    private final MemberService memberService;

    /**
     * GET
     * 회원 정보 조회
     */
    @GetMapping("/userProfile")
    public ResponseEntity<MemberResponse> getMyProfile(
            @AuthenticationPrincipal AuthUser authUser
    ) {
        MemberResponse myProfile = memberService.getMyProfile(authUser.memberId());
        return ResponseEntity.status(HttpStatus.OK).body(myProfile);
    }

    /**
     * PATCH
     * 회원 정보 수정
     */
    @PatchMapping("/userProfile/modify")
    public ResponseEntity<MemberResponse> updateProfile(
            @AuthenticationPrincipal AuthUser authuser,
            @RequestBody @Valid updateProfileRequest request
    ) {
        MemberResponse updatedProfile = memberService.updateProfile(authuser.memberId(), request);
        return ResponseEntity.status(HttpStatus.OK).body(updatedProfile);
    }

    /**
     * PATCH
     * 비밀번호 수정
     */
    @PatchMapping("/userProfile/password")
    public ResponseEntity<String> updatePassword(
            @AuthenticationPrincipal AuthUser authUser,
            @RequestBody @Valid updatePasswordRequest request
    ) {
        String successMessage = memberService.updatePassword(authUser.memberId(), request);
        return ResponseEntity.status(HttpStatus.OK).body(successMessage);
    }

    /**
     * DELETE
     * 회원 탈퇴
     */
    @DeleteMapping("/withdraw")
    public ResponseEntity<String> withdrawMember(
            @AuthenticationPrincipal AuthUser authUser,
            @RequestBody @Valid MemberWithdrawRequest request
    ) {
        String resultMessage = memberService.withdrawMember(authUser.memberId(), request);
        return ResponseEntity.status(HttpStatus.OK).body(resultMessage);
    }
}
