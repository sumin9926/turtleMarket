package turtleMart.security;

import io.jsonwebtoken.Claims;
import io.jsonwebtoken.JwtBuilder;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;
import io.jsonwebtoken.security.Keys;
import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import turtleMart.member.entity.Authority;

import java.security.Key;
import java.util.Base64;
import java.util.Date;

@Component
@RequiredArgsConstructor
public class JwtUtil {

    private final JwtSecurityProperties properties;
    private Key key;
    private final SignatureAlgorithm signatureAlgorithm = SignatureAlgorithm.HS256;

    @PostConstruct
    public void init() {
        String secretKey = properties.secret().key();
        String fixedKey = secretKey.replaceAll("\\s+", "");
        byte[] bytes = Base64.getDecoder().decode(fixedKey);
        key = Keys.hmacShaKeyFor(bytes);
    }

    public String createToken(Long memberId, Authority authority) {
        String payload = memberId + ":" + authority;
        return generateToken(payload, properties.token().prefix(), properties.token().expiration());
    }

    public String substringToken(String token) {
        String prefix = properties.token().prefix();
        if (StringUtils.hasText(token) && token.startsWith(prefix)) {
            return token.substring(prefix.length()).trim();
        }
        throw new IllegalArgumentException("Token 을 찾을 수 없습니다.");
    }

    public Claims extractClaims(String token) {
        return Jwts.parserBuilder()
                .setSigningKey(key)
                .build()
                .parseClaimsJws(token)
                .getBody();
    }

    public String removePrefix(String token) {
        return token.substring(properties.token().prefix().length()).trim();
    }

    private String generateToken(String payload, String prefix, long expiration) {
        Date date = new Date();
        JwtBuilder jwtBuilder = Jwts.builder()
                .setSubject(payload)
                .setExpiration(new Date(date.getTime() + expiration))
                .setIssuedAt(date) // 발급일
                .signWith(key, signatureAlgorithm);// 암호화 알고리즘

        return prefix + jwtBuilder.compact();
    }
}
