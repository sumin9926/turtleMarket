package turtleMart.order.service;

import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import turtleMart.global.exception.ErrorCode;
import turtleMart.global.exception.ForbiddenException;
import turtleMart.global.exception.NotFoundException;
import turtleMart.global.kafka.util.KafkaSendHelper;
import turtleMart.member.repository.MemberRepository;
import turtleMart.member.repository.SellerRepository;
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

    @Value("${kafka.topic.refund.approve}")
    private String refundApproveTopic;

    private final MemberRepository memberRepository;
    private final OrderItemRepository orderItemRepository;
    private final OrderItemDslRepository orderItemDslRepository;
    private final SellerRepository sellerRepository;
    private final KafkaSendHelper kafkaSendHelper;

    @Transactional
    public RefundApplyResultResponse requestRefund(Long memberId, Long orderItemId) {
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
        if (!sellerRepository.existsById(sellerId)) {
            throw new NotFoundException(ErrorCode.SELLER_NOT_FOUND);
        }

        List<RefundResponse> responseList = orderItemDslRepository.findAllOrderItemBySellerIdAndStatusRefunding(sellerId, OrderItemStatus.REFUNDING);

        if (responseList.isEmpty()) {
            throw new NotFoundException(ErrorCode.NO_REFUNDING_ORDER_ITEM_FOUND);
        }

        return responseList;
    }

    public void approveOrderItemRefund(Long orderItemId, Long sellerId) {
        if (!sellerRepository.existsById(sellerId)) {
            throw new NotFoundException(ErrorCode.SELLER_NOT_FOUND);
        }

        // 해당 상품의 판매자가 환불을 승인하려는 판매자와 동일인물인지 체크
        if(!orderItemRepository.existsByIdAndProductOptionCombination_Product_SellerId(orderItemId, sellerId)){
            throw new ForbiddenException(ErrorCode.PRODUCT_NOT_BELONG_TO_SELLER);
        }

        kafkaSendHelper.send(refundApproveTopic, orderItemId.toString(), orderItemId.toString());
        /* 결제 쪽:
         * 결제쪽에서 환불 완료 후 OrderItemStatus 상태를 Refunded 로 변경 해줘야한다.
         * 결제쪽에서 상태 변경 후 레디스 키("refund:status:" + orderItemId)도 true 로 변경해줘야한다 */
    }
}
