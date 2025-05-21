package turtleMart.payment.entity;

public enum FailReason {
    USER_CANCELLED,         // 유저가 취소한 결제
    INSUFFICIENT_FUNDS,     // 잔액 부족
    LIMIT_EXCEEDED,         // 한도 초과
    EXPIRED_CARD,           // 카드 유효기간 만료
    INVALID_CARD,           // 카드 정보 오류
    STOLEN_CARD,            // 도난 카드
    FRAUD_SUSPECTED,        // 사기 의심
    NETWORK_ERROR,          // 네트워크 오류
    PROCESSING_ERROR,       // 결제사 내부 처리 오류
    BANK_UNAVAILABLE;       // 은행 점검 등 이용 불가
}
