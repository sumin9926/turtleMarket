package turtleMart.member.dto.response;

import turtleMart.member.entity.Authority;
import turtleMart.member.entity.Member;
import turtleMart.member.entity.Membership;

public record MemberResponse(
        Long memberId,
        String name,
        String email,
        String phoneNumber,
        Membership membership,
        Authority authority
) {
    public static MemberResponse from(Member member) {
        return new MemberResponse(
                member.getId(),
                member.getName(),
                member.getEmail(),
                member.getPhoneNumber(),
                member.getMemberShip(),
                member.getAuthority()
        );
    }
}
