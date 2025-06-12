package turtleMart.member.dto.request;

import jakarta.validation.constraints.NotBlank;

public record UpdateSellerRequest(
        @NotBlank(message = "비밀번호를 입력해주세요.")
        String password,
        String businessName,
        String businessAddress,
        String account,
        String businessLicense
) {
}
