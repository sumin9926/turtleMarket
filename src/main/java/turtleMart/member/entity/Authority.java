package turtleMart.member.entity;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

import java.util.Arrays;

@Getter
@RequiredArgsConstructor
public enum Authority {
    CUSTOMER(Role.CUSTOMER, "고객"),
    SELLER(Role.SELLER, "판매자"),
    ADMIN(Role.ADMIN, "관리자");

    private final String role;
    private final String description;

    public static Authority of(String role) {
        return Arrays.stream(Authority.values())
                .filter(a -> a.name().equalsIgnoreCase(role))
                .findFirst()
                .orElseThrow(() -> new RuntimeException());
    }

    private static class Role {
        public static final String CUSTOMER = "ROLE_CUSTOMER";
        public static final String SELLER = "ROLE_SELLER";
        public static final String ADMIN = "ROLE_ADMIN";
    }
}
