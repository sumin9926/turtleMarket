package turtleMart.security;

import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;
import turtleMart.member.entity.Authority;

import java.nio.file.AccessDeniedException;

@Aspect
@Component
public class RoleCheckAspect {

    @Before("@annotation(checkRole)")
    public void checkUserRole(CheckRole checkRole) throws AccessDeniedException {
        String value = checkRole.value();

        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();

        if (authentication == null || !authentication.isAuthenticated()) {
            throw new AccessDeniedException("인증되지 않은 사용자입니다.");
        }

        AuthUser authUser = (AuthUser) authentication.getPrincipal();

        Authority requiredRole = Authority.of(value);

        if (!authUser.hasAuthority(requiredRole)) {
            throw new AccessDeniedException("권한이 없습니다. 필요한 권한: " + requiredRole.getDescription());
        }
    }
}
