package turtleMart.security;

import org.springframework.boot.context.properties.ConfigurationProperties;

import java.util.List;

@ConfigurationProperties(prefix = "jwt")
public record JwtSecurityProperties(
        Secret secret,
        Token token
) {

    public record Secret(
            String key,
            List<String> whiteList
    ) {}

    public record Token(
            String prefix,
            long expiration
    ) {}
}
