package turtleMart.member.dto.request;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;

public record updatePasswordRequest(
        @NotBlank(message = "비밀번호를 입력해주세요.")
        String oldPassword,
        @NotBlank(message = "새로운 비밀번호를 입력해주세요.")
        @Size(min = 8, max = 15, message = "비밀번호는 8자 이상 15자 이내여야 합니다.")
        String newPassword
) {
}
