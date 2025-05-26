package turtleMart.global.exception;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;

@Getter
@RequiredArgsConstructor
public enum ErrorCode {

    // 출고지(물류센터) 관련
    SENDER_NOT_FOUND(HttpStatus.NOT_FOUND, "존재하지 않는 출고지(물류센터)입니다."),
    SENDER_ALREADY_EXISTS(HttpStatus.CONFLICT, "이미 존재하는 출고지(물류센터)입니다."),

    // 택배사 관련
    COURIER_NOT_FOUND(HttpStatus.NOT_FOUND, "존재하지 않는 택배사입니다."),
    COURIER_ALREADY_EXISTS(HttpStatus.CONFLICT, "이미 존재하는 택배사입니다."),
    COURIER_DELETE_FAILED(HttpStatus.CONFLICT, "계약된 출고지(물류센터)가 존재하기 때문에 삭제할 수 없습니다."),

    // 배송 관련
    DELIVERY_NOT_FOUND(HttpStatus.NOT_FOUND, "존재하지 않는 배송입니다."),
    INVALID_DELIVERY_STATUS(HttpStatus.BAD_REQUEST, "허용되지 않은 상태 변경입니다."),

    // 주문 관련
    ORDER_NOT_FOUND(HttpStatus.NOT_FOUND, "존재하지 않는 주문입니다."),

    // 판매자 관련
    SELLER_NOT_FOUND(HttpStatus.NOT_FOUND, "존재하지 않는 판매자입니다."),

    // 주소 관련
    ADDRESS_NOT_FOUND(HttpStatus.NOT_FOUND, "존재하지 않는 주소입니다."),

    //권한 관련
    FORBIDDEN(HttpStatus.FORBIDDEN,"접근권한이 없습니다."),

    //상품 관련
    PRODUCT_NOT_FOUND(HttpStatus.NOT_FOUND,"존재하지 않는 상품입니다."),
    PRODUCT_ALL_READY_SURVIVE(HttpStatus.BAD_REQUEST, "해당 상품은 삭제되지않았습니다"),
    PRODUCT_OPTION_GROUP_NOT_FOUND(HttpStatus.NOT_FOUND, "존재하지 않는 상품옵션그룹입니다."),
    PRODUCT_OPTION_VALUE_NOT_FOUND(HttpStatus.NOT_FOUND, "상품옵션그룹에 존재하지않는 상품옵션값입니다."),

    //유저 관련
    MEMBER_NOT_FOUND(HttpStatus.NOT_FOUND, "존재하지 않는 유저입니다.");

    private final HttpStatus httpStatus;
    private final String message;
}
