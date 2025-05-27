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
import turtleMart.member.entity.Seller;
import turtleMart.member.repository.SellerRepository;
import turtleMart.product.dto.PromoteOptionGroupDto;
import turtleMart.product.dto.request.ApproveOptionRequest;
import turtleMart.product.dto.request.RequestOptionGroupRequest;
import turtleMart.product.dto.response.ProductOptionGroupResponse;
import turtleMart.product.dto.response.RequestOptionGroupResponse;
import turtleMart.product.entity.RequestOptionGroup;
import turtleMart.product.entity.RequestOptionValue;
import turtleMart.product.repository.RequestOptionGroupDslRepository;
import turtleMart.product.repository.RequestOptionGroupRepository;
import turtleMart.security.AuthUser;

import java.util.List;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class RequestOptionGroupService {
    private final RequestOptionGroupRepository requestOptionGroupRepository;
    private final RequestOptionValueService requestOptionValueService;
    private final SellerRepository sellerRepository;
    private final RequestOptionGroupDslRepository requestOptionGroupDslRepository;
    private final ProductOptionGroupService productOptionGroupService;

    @Transactional
    public RequestOptionGroupResponse createRequestOptionGroup(Long memberId, RequestOptionGroupRequest requestOptionGroupRequest) {
        Seller seller = sellerRepository.findByMemberId(memberId).orElseThrow(() -> new NotFoundException(ErrorCode.SELLER_NOT_FOUND));
        RequestOptionGroup requestOptionGroup = RequestOptionGroup.of(requestOptionGroupRequest.name(), seller);
        requestOptionValueService.createRequestOptionValue(requestOptionGroup, requestOptionGroupRequest.valueNameList());
        requestOptionGroupRepository.save(requestOptionGroup);
        return RequestOptionGroupResponse.from(requestOptionGroup);
    }

    public Page<RequestOptionGroupResponse> getAllRequestOptionGroupBySeller(Long memberId, Pageable pageable) {
        Seller seller = sellerRepository.findByMemberId(memberId).orElseThrow(() -> new NotFoundException(ErrorCode.SELLER_NOT_FOUND));
        Page<RequestOptionGroup> requestOptionGroupPage = requestOptionGroupDslRepository.findAllBySellerIdWithValue(seller.getId(),pageable);
        return requestOptionGroupPage.map(RequestOptionGroupResponse::from);
    }

    public Page<RequestOptionGroupResponse> getAllRequestOptionGroupWithNotYet(Pageable pageable) {
        return requestOptionGroupDslRepository.findAllByNotYetWithSeller(pageable).map(RequestOptionGroupResponse::from);
    }

    public Page<RequestOptionGroupResponse> getAllRequestOptionGroupWithAllReady(Pageable pageable) {
        return requestOptionGroupDslRepository.findAllByAllReadyWithSeller(pageable).map(RequestOptionGroupResponse::from);
    }

    public ProductOptionGroupResponse approveRequestOptionGroup(AuthUser authUser, ApproveOptionRequest approveOptionRequest) {
        if (!authUser.hasAuthority(Authority.ADMIN)) {
            throw new RoleMismatchException(ErrorCode.FORBIDDEN);
        }
        RequestOptionGroup requestOptionGroup = requestOptionGroupDslRepository.findByIdWithValue(approveOptionRequest.requestOptionGroupId());
        List<RequestOptionValue> requestOptionValues = requestOptionGroup.extractSelectValue(approveOptionRequest.requestOptionValueIdList());
        PromoteOptionGroupDto promoteOptionGroupDto = PromoteOptionGroupDto.of(requestOptionGroup, requestOptionValues);
        return productOptionGroupService.promoteOptionGroup(promoteOptionGroupDto);
    }

    public void deleteRequestOptionGroup(Long memberId, Long requestOptionGroupId) {
        Seller seller = sellerRepository.findByMemberId(memberId).orElseThrow(() -> new NotFoundException(ErrorCode.SELLER_NOT_FOUND));
        RequestOptionGroup requestOptionGroup =
                requestOptionGroupRepository.findById(requestOptionGroupId).orElseThrow(() -> new NotFoundException(ErrorCode.REQUEST_OPTION_GROUP_NOT_FOUND));

        if (!requestOptionGroup.getSeller().getId().equals(seller.getId())) {
            throw new RoleMismatchException(ErrorCode.FORBIDDEN);
        }

        //밸류의 상태를 검증하고 특정 상태인것들은 삭제하지 못하도록 막을까 했으나
        //판매자가 신청한 옵션그룹과 밸류를 그렇게 철저하게 관리할 필요가 있을까 라고 한다면 그것은 리소스 낭비일것이라 판단함
        //그리하여 하드딜리트하도록 설정
        requestOptionGroupRepository.delete(requestOptionGroup);
    }
}
