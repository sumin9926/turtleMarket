package turtleMart.member.service;

import lombok.RequiredArgsConstructor;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import turtleMart.member.dto.request.DeleteSellerRequest;
import turtleMart.member.dto.request.SellerRegisterRequest;
import turtleMart.member.dto.request.UpdateSellerRequest;
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
    private final PasswordEncoder passwordEncoder;

    /**
     * 판매자 등록
     */
    public SellerResponse registerSeller(Long authId, SellerRegisterRequest request) {
        Member foundMember = memberRepository.findById(authId)
                .orElseThrow(() -> new RuntimeException(""));
        Seller seller = Seller.of(request, foundMember);
        sellerRepository.save(seller);
        return SellerResponse.from(seller);
    }

    /**
     * 판매자 조회
     */
    public SellerResponse findSellerInfo(Long sellerId) {
        Seller foundSeller = findSeller(sellerId);
        return SellerResponse.from(foundSeller);
    }

    /**
     * 판매자 조회(판매자 전용)
     */
    public SellerResponse findMySellerProfile(Long authId, Long sellerId) {
        Seller foundSeller = findSeller(sellerId);
        validSellerById(authId, foundSeller);
        return SellerResponse.from(foundSeller);
    }

    /**
     * 판매자 수정
     */
    public SellerResponse updateSellerProfile(Long authId, Long sellerId, UpdateSellerRequest request) {
        Seller foundSeller = findSeller(sellerId);
        validSellerById(authId, foundSeller);
        validPassword(request.password(), foundSeller);
        foundSeller.updateSeller(request);
        sellerRepository.save(foundSeller);
        return SellerResponse.from(foundSeller);
    }

    /**
     * 판매자 삭제
     */
    public String deleteSeller(Long authId, Long sellerId, DeleteSellerRequest request) {
        Seller foundSeller = findSeller(sellerId);
        validSellerById(authId, foundSeller);
        validPassword(request.password(), foundSeller);
        sellerRepository.delete(foundSeller);
        return "판매자 해지가 완료되었습니다.";
    }

    private Seller findSeller(Long sellerId) {
        return sellerRepository.findById(sellerId)
                .orElseThrow(() -> new RuntimeException("판매자를 찾을 수 없습니다."));
    }

    private static void validSellerById(Long authId, Seller seller) {
        if (!seller.getMember().getId().equals(authId)) {
            throw new RuntimeException("판매자로 등록된 회원이 아닙니다.");
        }
    }

    private void validPassword(String requestPassword, Seller seller) {
        if (!passwordEncoder.matches(requestPassword, seller.getMember().getPassword())) {
            throw new RuntimeException("비밀번호가 일치하지 않습니다.");
        }
    }
}
