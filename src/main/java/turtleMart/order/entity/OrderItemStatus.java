package turtleMart.order.entity;

public enum OrderItemStatus {
    UNPAID, //미결재
    CANCELED, //결재 실패
    ORDERED, //결재 완료
    REFUNDING, //환불 신청 중
    REFUNDED, //환불됨
    REJECTED, //환불 거부
    CONFIRMED; //구매 확정됨

    public Boolean canTransitionTo(OrderItemStatus target){
        return switch (this) {
            case UNPAID -> target == OrderItemStatus.CANCELED || target == OrderItemStatus.ORDERED;
            case CANCELED -> target == OrderItemStatus.ORDERED;
            case ORDERED -> target == OrderItemStatus.REFUNDING || target == OrderItemStatus.CONFIRMED;
            case REFUNDING -> target == OrderItemStatus.REFUNDED || target == OrderItemStatus.REJECTED;
            case REFUNDED, CONFIRMED, REJECTED -> false;
        };
    }

    public void validateTransitionTo(OrderItemStatus target){
        if(!this.canTransitionTo(target)){
            throw new IllegalStateException(
                    String.format("상태 변경 불가: 현재 상태 '%s'에서 '%s'(으)로 변경할 수 없습니다.", this, target)
            );
        }
    }
}
