package turtleMart.member.dto.request;

import jakarta.validation.constraints.NotBlank;

public record SellerRegisterRequest(
        @NotBlank(message = "상호명을 입력해주세요.")
        String businessName,
        @NotBlank(message = "사업장 주소를 입력해주세요.")
        String businessAddress,
        @NotBlank(message = "계좌번호를 입력해주세요.")
        String account,
        @NotBlank(message = "사업자 등록번호를 입력해주세요.")
        String businessLicense
) {
}
