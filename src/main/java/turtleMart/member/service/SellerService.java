package turtleMart.member.service;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import turtleMart.member.dto.request.SellerRegisterRequest;
import turtleMart.member.dto.response.SellerResponse;
import turtleMart.member.entity.Member;
import turtleMart.member.entity.Seller;
import turtleMart.member.repository.MemberRepository;
import turtleMart.member.repository.SellerRepository;

@Service
@RequiredArgsConstructor
public class SellerService {
    private final SellerRepository sellerRepository;
    private final MemberRepository memberRepository;

    public SellerResponse registerSeller(Long authId, SellerRegisterRequest request) {
        Member foundMember = memberRepository.findById(authId)
                .orElseThrow(() -> new RuntimeException(""));
        Seller seller = Seller.of(request, foundMember);
        sellerRepository.save(seller);
        return SellerResponse.from(seller);
    }

    private Seller findSeller(Long sellerId) {
        return sellerRepository.findById(sellerId)
                .orElseThrow(() -> new RuntimeException("판매자를 찾을 수 없습니다."));
    }

    public SellerResponse findSellerInfo(Long sellerId) {
        Seller foundSeller = findSeller(sellerId);
        return SellerResponse.from(foundSeller);
    }

    public SellerResponse findMySellerProfile(Long authId, Long sellerId) {
        Seller foundSeller = findSeller(sellerId);
        if (!foundSeller.getMember().getId().equals(authId)) {
            throw new RuntimeException("판매자로 등록된 회원이 아닙니다.");
        }
        return SellerResponse.from(foundSeller);
    }
}
