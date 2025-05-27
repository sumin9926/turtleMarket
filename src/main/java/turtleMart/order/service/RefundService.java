package turtleMart.order.service;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import turtleMart.global.exception.BadRequestException;
import turtleMart.global.exception.ErrorCode;
import turtleMart.global.exception.NotFoundException;
import turtleMart.member.repository.MemberRepository;
import turtleMart.order.dto.response.RefundApplyResultResponse;
import turtleMart.order.entity.OrderItem;
import turtleMart.order.entity.OrderItemStatus;
import turtleMart.order.repository.OrderItemRepository;

@Service
@RequiredArgsConstructor
public class RefundService {

    private final MemberRepository memberRepository;
    private final OrderItemRepository orderItemRepository;

    @Transactional
    public RefundApplyResultResponse applyRefund(Long memberId, Long orderItemId) {
        if (!memberRepository.existsById(memberId)) {
            throw new NotFoundException(ErrorCode.MEMBER_NOT_FOUND);
        }

        OrderItem orderItem = orderItemRepository.findById(orderItemId)
                .orElseThrow(() -> new NotFoundException(ErrorCode.ORDER_ITEM_NOT_FOUND));

        if (!orderItem.getOrder().getMember().getId().equals(memberId)) {
            throw new BadRequestException(ErrorCode.ORDER_ITEM_NOT_OWNED_BY_MEMBER);
        }

        //갱신 가능 여부 체크
        orderItem.getOrderItemStatus().validateTransitionTo(OrderItemStatus.REFUNDING);
        orderItem.updateStatus(OrderItemStatus.REFUNDING);

        return RefundApplyResultResponse.from(orderItem);
    }
}
