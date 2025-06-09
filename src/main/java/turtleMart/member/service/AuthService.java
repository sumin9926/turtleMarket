package turtleMart.member.service;

import lombok.RequiredArgsConstructor;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import turtleMart.global.exception.BadRequestException;
import turtleMart.global.exception.ErrorCode;
import turtleMart.global.exception.NotFoundException;
import turtleMart.member.dto.request.LoginRequest;
import turtleMart.member.dto.request.SignupRequest;
import turtleMart.member.dto.response.TokenResponse;
import turtleMart.member.entity.Member;
import turtleMart.member.repository.MemberRepository;
import turtleMart.security.JwtUtil;

@Service
@RequiredArgsConstructor
public class AuthService {

    private final MemberRepository memberRepository;
    private final PasswordEncoder passwordEncoder;
    private final JwtUtil jwtUtil;

    /**
     * 회원가입
     */
    @Transactional
    public TokenResponse signup(SignupRequest request) {
        if (memberRepository.existsByEmail(request.email())) {
//            throw new RuntimeException("이미 가입된 이메일입니다.");
            throw new BadRequestException(ErrorCode.EMAIL_ALREADY_EXIST);
        }
        Member member = Member.of(request, passwordEncoder.encode(request.password()));
        memberRepository.save(member);
        String token = createToken(member);
        return TokenResponse.from("회원가입이 완료되었습니다.", token);
    }

    /**
     * 로그인
     */
    @Transactional
    public TokenResponse login(LoginRequest request) {
        Member member = memberRepository.findMemberByEmail(request.email())
//                .orElseThrow(() -> new RuntimeException("가입된 이메일이 아닙니다."));
                .orElseThrow(() -> new NotFoundException(ErrorCode.EMAIL_NOT_FOUND));
        if (!passwordEncoder.matches(request.password(), member.getPassword())) {
//            throw new RuntimeException("비밀번호가 잘못되었습니다.");
            throw new BadRequestException(ErrorCode.INVALID_PASSWORD);
        }
        String token = createToken(member);
        return TokenResponse.from("로그인이 완료되었습니다.", token);
    }

    private String createToken(Member member) {
        String token = jwtUtil.createToken(member.getId(), member.getAuthority());
        return jwtUtil.removePrefix(token);
    }
}
