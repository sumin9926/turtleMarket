package turtleMart.member.conrtroller;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
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

    /**
     * PATCH
     * 비밀번호 수정
     */

    /**
     * DELETE
     * 회원 탈퇴
     */

}
