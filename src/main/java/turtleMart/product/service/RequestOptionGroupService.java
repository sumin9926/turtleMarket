package turtleMart.product.service;

import org.springframework.stereotype.Service;
import turtleMart.product.dto.request.ApproveOptionRequest;
import turtleMart.product.dto.request.RequestOptionGroupRequest;
import turtleMart.product.dto.response.ProductOptionGroupResponse;
import turtleMart.product.dto.response.RequestOptionGroupResponse;
import turtleMart.security.AuthUser;

import java.util.List;

@Service
public class RequestOptionGroupService {
    public RequestOptionGroupResponse createRequestOptionGroup(Long memberId, RequestOptionGroupRequest requestOptionGroupRequest) {
        return null;
    }

    public List<RequestOptionGroupResponse> getAllRequestOptionGroupBySeller(Long memberId) {
        return null;
    }

    public List<RequestOptionGroupResponse> getAllRequestOptionGroupWithNotYet() {
        return null;
    }

    public List<RequestOptionGroupResponse> getAllRequestOptionGroupWithAllReady() {
        return null;
    }

    public void deleteRequestOptionGroup(Long memberId, Long requestOptionGroupId) {

    }

    public ProductOptionGroupResponse approveRequestOptionGroup(AuthUser authUser, ApproveOptionRequest approveOptionRequest) {
        return null;
    }
}
