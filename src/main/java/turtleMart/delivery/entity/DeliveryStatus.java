package turtleMart.delivery.entity;

public enum DeliveryStatus {

    PREPARING,  // 배송 준비 중
    SHIPPED,  // 택배사에 인계 완료 (출고)
    IN_TRANSIT,  // 배송 중 (택배사에서 이동 중)
    DELIVERED,  // 배송 완료 (고객 수령)
    FAILED,  // 배송 실패 (수령 불가, 주소 오류 등)
    CANCELLED  // 배송 취소 (주문 취소 등으로 인한 무료 처리)
}
