package turtleMart.member.service;

import lombok.RequiredArgsConstructor;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import turtleMart.member.dto.request.updatePasswordRequest;
import turtleMart.member.dto.request.updateProfileRequest;
import turtleMart.member.dto.response.MemberResponse;
import turtleMart.member.entity.Member;
import turtleMart.member.repository.MemberRepository;

@Service
@RequiredArgsConstructor
public class MemberService {
    private final MemberRepository memberRepository;
    private final PasswordEncoder passwordEncoder;

    public MemberResponse getMyProfile(Long authId) {
        Member foundMember = getMember(authId);
        return MemberResponse.from(foundMember);
    }

    public MemberResponse updateProfile(Long authId, updateProfileRequest request) {
        Member foundMember = getMember(authId);
        verifyPassword(request.password(), foundMember);
        foundMember.updateProfile(request);
        memberRepository.save(foundMember);
        return MemberResponse.from(foundMember);
    }

    public String updatePassword(Long authId, updatePasswordRequest request) {
        Member foundMember = getMember(authId);
        verifyPassword(request.oldPassword(), foundMember);
        String encodedpassword = passwordEncoder.encode(request.newPassword());
        foundMember.updatePassword(encodedpassword);
        memberRepository.save(foundMember);
        return "비밀번호가 변경되었습니다.";
    }

    private Member getMember(Long authId) {
        return memberRepository.findById(authId)
                .orElseThrow(() -> new RuntimeException(""));
    }

    private void verifyPassword(String password, Member member) {
        if (!passwordEncoder.matches(password, member.getPassword())) {
            throw new RuntimeException("비밀번호가 일치하지 않습니다.");
        }
    }
}
