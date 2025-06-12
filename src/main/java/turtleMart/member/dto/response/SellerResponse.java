package turtleMart.member.dto.response;

import turtleMart.member.entity.Authority;
import turtleMart.member.entity.Seller;

public record SellerResponse(
        Long sellerId,
        String SellerName,
        Authority authority,
        String businessName,
        String businessAddress,
        String account,
        String businessLicense
) {
    public static SellerResponse from(Seller seller) {
        return new SellerResponse(
                seller.getId(),
                seller.getMember().getName(),
                seller.getMember().getAuthority(),
                seller.getBusinessName(),
                seller.getBusinessAddress(),
                seller.getAccount(),
                seller.getBusinessLicense()
        );
    }
}
