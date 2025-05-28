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
import turtleMart.product.dto.PromoteOptionGroupDto;
import turtleMart.product.dto.request.ProductOptionGroupRequest;
import turtleMart.product.dto.request.ProductOptionGroupRequestUpdate;
import turtleMart.product.dto.request.ProductOptionValueRequest;
import turtleMart.product.dto.request.ProductOptionValueUpdateRequest;
import turtleMart.product.dto.response.ProductOptionGroupResponse;
import turtleMart.product.dto.response.ProductOptionGroupResponseUpdate;
import turtleMart.product.entity.ProductOptionGroup;
import turtleMart.product.repository.ProductOptionGroupDslRepository;
import turtleMart.product.repository.ProductOptionGroupRepository;

import java.util.List;
import java.util.Map;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class ProductOptionGroupService {

    private final MemberRepository memberRepository;
    private final ProductOptionGroupRepository productOptionGroupRepository;
    private final ProductOptionValueService productOptionValueService;
    private final ProductOptionGroupDslRepository productOptionGroupDslRepository;

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
        checkPermission(memberId);
        ProductOptionGroup productOptionGroup = ProductOptionGroup.of(productOptionGroupRequest.name());
        productOptionValueService.createProductOptionValue(productOptionGroupRequest.optionNameList(),productOptionGroup);
        productOptionGroupRepository.save(productOptionGroup);
        return ProductOptionGroupResponse.from(productOptionGroup);
    }

    public Page<ProductOptionGroupResponse> getAllProductOptionGroup(Pageable pageable) {
        return productOptionGroupDslRepository.findAllWithValue(pageable).map(ProductOptionGroupResponse::from);
    }

    @Transactional
    public ProductOptionGroupResponse updateProductOptionGroup(ProductOptionGroupRequestUpdate productOptionGroupRequest, Long memberId, Long productOptionGroupId) {
        //시큐리티 없을때 임시
        checkPermission(memberId);
        ProductOptionGroup productOptionGroup = productOptionGroupRepository.findById(productOptionGroupId)
                .orElseThrow(() -> new NotFoundException(ErrorCode.PRODUCT_OPTION_GROUP_NOT_FOUND));
        productOptionGroup.update(productOptionGroupRequest);
        return ProductOptionGroupResponse.from(productOptionGroup);
    }

    @Transactional
    public void deleteProductOptionGroup(Long productOptionGroupId, Long memberId) {
        checkPermission(memberId);
        ProductOptionGroup productOptionGroup = productOptionGroupRepository.findById(productOptionGroupId)
                .orElseThrow(() -> new NotFoundException(ErrorCode.PRODUCT_OPTION_GROUP_NOT_FOUND));

        // 옵션그룹에 포함된 옵션이 상품과 연결되있으면 삭제못하도록 검증

        productOptionGroupRepository.delete(productOptionGroup);
    }

    @Transactional
    public ProductOptionGroupResponse updateProductOptionValue(List<ProductOptionValueUpdateRequest> productOptionValueRequest, Long memberId, Long productOptionGroupId) {
        checkPermission(memberId);
        ProductOptionGroup productOptionGroup = productOptionGroupRepository.findById(productOptionGroupId)
                .orElseThrow(() -> new NotFoundException(ErrorCode.PRODUCT_OPTION_GROUP_NOT_FOUND));
        for (ProductOptionValueUpdateRequest optionValueRequest : productOptionValueRequest) {
            productOptionGroup.updateOptionValue(optionValueRequest);
        }
        return ProductOptionGroupResponse.from(productOptionGroup);
    }

    private void checkPermission(Long memberId) {
        Member member = memberRepository.findById(memberId).orElseThrow(() -> new NotFoundException(ErrorCode.MEMBER_NOT_FOUND));
        if (!member.getAuthority().equals(Authority.ADMIN)) {
            throw new RoleMismatchException(ErrorCode.FORBIDDEN);
        }
    }

    @Transactional
    public ProductOptionGroupResponseUpdate createProductOptionValue(List<ProductOptionValueRequest> productOptionValueRequest, Long memberId, Long productOptionGroupId) {
        checkPermission(memberId);
        ProductOptionGroup productOptionGroup = productOptionGroupRepository.findById(productOptionGroupId)
                .orElseThrow(() -> new NotFoundException(ErrorCode.PRODUCT_OPTION_GROUP_NOT_FOUND));
        Map<String, String> passList = productOptionValueService.updateProductOptionValue(productOptionValueRequest, productOptionGroup);
        return ProductOptionGroupResponseUpdate.of(productOptionGroup,passList);
    }

    @Transactional
    public void deleteProductOptionValue(List<Long> productOptionValueList, Long memberId, Long productOptionGroupId) {
        checkPermission(memberId);
        ProductOptionGroup productOptionGroup = productOptionGroupRepository.findById(productOptionGroupId)
                .orElseThrow(() -> new NotFoundException(ErrorCode.PRODUCT_OPTION_GROUP_NOT_FOUND));
        // 옵션밸류값이 상품과 연결되있지않을경우검증

        for (Long productOptionValueId : productOptionValueList) {
            productOptionGroup.deleteValue(productOptionValueId);
        }

    }

    @Transactional
    public ProductOptionGroupResponse promoteOptionGroup(PromoteOptionGroupDto promoteOptionGroupDto) {
        ProductOptionGroup productOptionGroup = ProductOptionGroup.of(promoteOptionGroupDto.optionGroupName());
        productOptionValueService.createProductOptionValue(promoteOptionGroupDto.optionValueNameList(), productOptionGroup);
        productOptionGroupRepository.save(productOptionGroup);
        return ProductOptionGroupResponse.from(productOptionGroup);
    }

}
