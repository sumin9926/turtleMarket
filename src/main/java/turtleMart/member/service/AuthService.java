package turtleMart.member.service;

import lombok.RequiredArgsConstructor;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import turtleMart.member.dto.request.SignupRequest;
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
    public String signup(SignupRequest request) {
        if (memberRepository.existsByEmail(request.email())) {
            throw new RuntimeException("이미 가입된 이메일입니다.");
        }
        Member member = Member.of(request, passwordEncoder.encode(request.password()));
        memberRepository.save(member);
        String token = jwtUtil.createToken(member.getId(), member.getAuthority());
        return jwtUtil.removePrefix(token);
    }
}
