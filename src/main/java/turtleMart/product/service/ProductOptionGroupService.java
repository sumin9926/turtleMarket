package turtleMart.product.service;

import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import turtleMart.global.exception.ErrorCode;
import turtleMart.global.exception.NotFoundException;
import turtleMart.global.exception.RoleMismatchException;
import turtleMart.member.entity.Authority;
import turtleMart.member.entity.Member;
import turtleMart.member.repository.MemberRepository;
import turtleMart.member.repository.SellerRepository;
import turtleMart.product.dto.ProductOptionGroupRequest;
import turtleMart.product.dto.ProductOptionGroupResponse;
import turtleMart.product.dto.ProductOptionValueResponse;
import turtleMart.product.entity.OptionStatus;
import turtleMart.product.entity.ProductOptionGroup;
import turtleMart.product.repository.ProductOptionGroupRepository;

@Service
@RequiredArgsConstructor
public class ProductOptionGroupService {

    private final MemberRepository memberRepository;
    private final ProductOptionGroupRepository productOptionGroupRepository;
    private final SellerRepository sellerRepository;
    private final ProductOptionValueService productOptionValueService;

    @Transactional
    public ProductOptionGroupResponse createProductOptionGroup(ProductOptionGroupRequest productOptionGroupRequest, Long memberId) {
        //시큐리티 추가시
        /*
        if(!authUser.getRole().equals(ADMIN)){
            throw new RoleMismatchException(ErrorCode.FORBIDDEN);
        }
        if(!memberRepository.existsById(authUser.getMemberId())){
            throw new NotFoundException(ErrorCode.MEMBER_NOT_FOUND)
        }
         */
        //시큐리티 없을때 임시
        Member member = memberRepository.findById(memberId).orElseThrow(() -> new NotFoundException(ErrorCode.MEMBER_NOT_FOUND));
        if (!member.getAuthority().equals(Authority.ADMIN)) {
            throw new RoleMismatchException(ErrorCode.FORBIDDEN);
        }
        ProductOptionGroup productOptionGroup = ProductOptionGroup.of(productOptionGroupRequest.name());
        productOptionValueService.createProductOptionValue(productOptionGroupRequest.optionNameList(),productOptionGroup);
        productOptionGroupRepository.save(productOptionGroup);
        return ProductOptionGroupResponse.from(productOptionGroup);
    }

    public Page<ProductOptionGroupResponse> getAllProductOptionGroup(Pageable pageable) {
        return null;
    }

    public ProductOptionGroupResponse updateProductOptionGroup(Long memberId) {
        return null;
    }

    public void deleteProductOptionGroup(Long productOptionGroupId, Long memberId) {

    }
}
