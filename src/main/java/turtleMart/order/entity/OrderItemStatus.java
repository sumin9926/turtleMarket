package turtleMart.order.entity;

public enum OrderItemStatus {
    UNPAID, //미결재
    CANCELED, //결재 실패
    ORDERED, //결재 완료
    REFUNDING, //환불 신청 중
    REFUNDED, //환불됨
    CONFIRMED //구매 확정됨
}
