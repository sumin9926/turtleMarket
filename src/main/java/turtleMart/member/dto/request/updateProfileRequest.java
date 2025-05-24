package turtleMart.member.dto.request;

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Pattern;

public record updateProfileRequest(
        @NotBlank(message = "비밀번호를 입력해주세요.")
        String password,
        String name,
        @Email
        String email,
        @Pattern(regexp = "^\\d{3}-\\d{3,4}-\\d{4}$")
        String phoneNumber
) {
}
