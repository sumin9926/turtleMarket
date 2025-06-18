package turtleMart.member.dto.response;

public record TokenResponse(
        String message,
        String token
) {
    public static TokenResponse from(String message, String token) {
        return new TokenResponse(message, token);
    }
}
