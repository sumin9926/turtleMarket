package turtleMart.order.service;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import turtleMart.global.exception.ErrorCode;
import turtleMart.global.exception.ForbiddenException;
import turtleMart.global.exception.NotFoundException;
import turtleMart.member.repository.MemberRepository;
import turtleMart.order.dto.response.RefundApplyResultResponse;
import turtleMart.order.dto.response.RefundResponse;
import turtleMart.order.entity.OrderItem;
import turtleMart.order.entity.OrderItemStatus;
import turtleMart.order.repository.OrderItemDslRepository;
import turtleMart.order.repository.OrderItemRepository;

import java.util.List;

@Service
@RequiredArgsConstructor
public class RefundService {

    private final MemberRepository memberRepository;
    private final OrderItemRepository orderItemRepository;
    private final OrderItemDslRepository orderItemDslRepository;

    @Transactional
    public RefundApplyResultResponse applyRefund(Long memberId, Long orderItemId) {
        if (!memberRepository.existsById(memberId)) {
            throw new NotFoundException(ErrorCode.MEMBER_NOT_FOUND);
        }

        OrderItem orderItem = orderItemRepository.findById(orderItemId)
                .orElseThrow(() -> new NotFoundException(ErrorCode.ORDER_ITEM_NOT_FOUND));

        if (!orderItem.getOrder().getMember().getId().equals(memberId)) {
            throw new ForbiddenException(ErrorCode.ORDER_ITEM_NOT_OWNED_BY_MEMBER);
        }

        //갱신 가능 여부 체크
        orderItem.getOrderItemStatus().validateTransitionTo(OrderItemStatus.REFUNDING);
        orderItem.updateStatus(OrderItemStatus.REFUNDING);

        return RefundApplyResultResponse.from(orderItem);
    }

    public List<RefundResponse> getRefundRequestList(Long sellerId) {
        if (!memberRepository.existsById(sellerId)) {
            throw new NotFoundException(ErrorCode.SELLER_NOT_FOUND);
        }

        List<RefundResponse> responseList = orderItemDslRepository.findAllOrderItemBySellerIdAndStatusRefunding(sellerId, OrderItemStatus.REFUNDING);

        if(responseList.isEmpty()){
            throw new NotFoundException(ErrorCode.NO_REFUNDING_ORDER_ITEM_FOUND);
        }

        return responseList;
    }
}
