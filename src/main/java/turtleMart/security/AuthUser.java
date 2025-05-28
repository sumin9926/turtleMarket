package turtleMart.security;

import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import turtleMart.member.entity.Authority;

import java.util.Collection;
import java.util.List;

public record AuthUser(
        Long memberId,
        Collection<? extends GrantedAuthority> authority
) {

    public static AuthUser of(Long memberId, Authority authority) {
        return new AuthUser(
                memberId,
                List.of(new SimpleGrantedAuthority(authority.getRole()))
        );
    }

    public boolean hasAuthority(Authority targetAuthority) {
        return authority.stream()
                .anyMatch(auth -> auth.getAuthority().equals(targetAuthority.getRole()));
    }
}
