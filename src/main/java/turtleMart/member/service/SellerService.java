package turtleMart.member.service;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import turtleMart.member.dto.request.SellerRegisterRequest;
import turtleMart.member.dto.response.SellerResponse;
import turtleMart.member.entity.Member;
import turtleMart.member.entity.Seller;
import turtleMart.member.repository.MemberRepository;
import turtleMart.member.repository.SellerRepository;
import turtleMart.security.AuthUser;

import java.util.Optional;

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
}
