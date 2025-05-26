package turtleMart.member.conrtroller;

import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import turtleMart.member.dto.request.SignupRequest;
import turtleMart.member.dto.request.LoginRequest;
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
    public ResponseEntity<String> signup(
            @RequestBody @Valid SignupRequest request
    ) {
        String token = authService.signup(request);
        return ResponseEntity.status(HttpStatus.CREATED).body(token);
    }

    /**
     * POST
     * 로그인
     */
    @PostMapping("/login")
    public ResponseEntity<String> login(
            @RequestBody @Valid LoginRequest request
    ) {
        String token = authService.login(request);
        return ResponseEntity.status(HttpStatus.OK).body(token);
    }
}
