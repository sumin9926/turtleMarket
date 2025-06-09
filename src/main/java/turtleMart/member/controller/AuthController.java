package turtleMart.member.controller;

import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import turtleMart.member.dto.request.EmailLoginRequest;
import turtleMart.member.dto.request.PhoneNumberLoginRequest;
import turtleMart.member.dto.request.SignupRequest;
import turtleMart.member.dto.response.TokenResponse;
import turtleMart.member.service.AuthService;

@RestController
@RequestMapping("/auth")
@RequiredArgsConstructor
public class AuthController {

    private final AuthService authService;

    /**
     * POST
     * 회원가입
     */
    @PostMapping("/signup")
    public ResponseEntity<TokenResponse> signup(
            @RequestBody @Valid SignupRequest request
    ) {
        TokenResponse response = authService.signup(request);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    /**
     * POST
     * 이메일 로그인
     */
    @PostMapping("/login/email")
    public ResponseEntity<TokenResponse> emailLogin(
            @RequestBody @Valid EmailLoginRequest request
    ) {
        TokenResponse response = authService.emailLogin(request);
        return ResponseEntity.status(HttpStatus.OK).body(response);
    }

    /**
     * POST
     * 휴대폰 번호 로그인
     */
    @PostMapping("/login/phoneNumber")
    public ResponseEntity<TokenResponse> phoneNumberLogin(
            @RequestBody @Valid PhoneNumberLoginRequest request
    ) {
        TokenResponse response = authService.phoneNumberLogin(request);
        return ResponseEntity.status(HttpStatus.OK).body(response);
    }

    /**
     * POST
     * 로그아웃
     */
}
