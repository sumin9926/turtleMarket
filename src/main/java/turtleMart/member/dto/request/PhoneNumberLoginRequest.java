package turtleMart.member.dto.request;

import jakarta.validation.constraints.NotBlank;

public record PhoneNumberLoginRequest(
        @NotBlank(message = "휴대폰 번호를 입력해주세요.")
        String phoneNumber,
        @NotBlank(message = "비밀번호를 입력해주세요.")
        String password
) {
}
