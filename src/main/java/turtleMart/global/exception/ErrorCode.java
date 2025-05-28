package turtleMart.global.exception;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;

@Getter
@RequiredArgsConstructor
public enum ErrorCode {

    //출고지(물류센터) 관련
    SENDER_NOT_FOUND(HttpStatus.NOT_FOUND, "존재하지 않는 출고지(물류센터)입니다."),

    //택배사 관련
    COURIER_NOT_FOUND(HttpStatus.NOT_FOUND, "존재하지 않는 택배사입니다."),

    // 배송 관련
    DELIVERY_NOT_FOUND(HttpStatus.NOT_FOUND, "존재하지 않는 배송입니다."),

    // 샐러 관련
    SELLER_NOT_FOUND(HttpStatus.NOT_FOUND,"존재하지 않는 판매자입니다."),

    //권한 관련
    FORBIDDEN(HttpStatus.FORBIDDEN,"접근권한이 없습니다."),

    //상품 관련
    PRODUCT_NOT_FOUND(HttpStatus.NOT_FOUND,"존재하지 않는 상품입니다."),
    PRODUCT_ALL_READY_SURVIVE(HttpStatus.BAD_REQUEST, "해당 상품은 삭제되지않았습니다"),
    PRODUCT_OPTION_GROUP_NOT_FOUND(HttpStatus.NOT_FOUND, "존재하지 않는 상품옵션그룹입니다."),
    PRODUCT_OPTION_VALUE_NOT_FOUND(HttpStatus.NOT_FOUND, "상품옵션그룹에 존재하지않는 상품옵션값입니다."),
    PRODUCT_OPTION_COMBINATION_ALL_READY_SOLD(HttpStatus.BAD_REQUEST, "주문이 존재하는 상품옵션조합은 삭제할수없습니다."),
    PRODUCT_OPTION_COMBINATION_NOT_FOUND(HttpStatus.NOT_FOUND, "존재하지 않는 상품옵션조합입니다."),

    //주문 관련
    ORDER_SHEET_NOT_FOUND(HttpStatus.NOT_FOUND, "주문서 내용이 존재하지 않습니다."),
    ORDER_NOT_FOUND(HttpStatus.NOT_FOUND, "주문 내역이 존재하지 않습니다."),
    ORDER_ITEM_NOT_FOUND(HttpStatus.NOT_FOUND, "상품 주문 내역이 존재하지 않습니다."),
    ORDER_ITEM_NOT_IN_ORDER(HttpStatus.BAD_REQUEST, "해당 주문에 속하지 않은 상품입니다."),
    ORDER_ITEM_NOT_OWNED_BY_MEMBER(HttpStatus.FORBIDDEN, "해당 회원이 이 주문 항목의 소유자가 아닙니다."),
    NO_REFUNDING_ORDER_ITEM_FOUND(HttpStatus.NOT_FOUND, "환불중인 주문 항목이 없습니다."),
    ORDER_PRICE_VALIDATION_FAILED(HttpStatus.CONFLICT, "주문 생성 중 가격 정합성 오류: 주문서 가격과 상품 가격이 일치하지 않습니다."),

    //장바구니 관련
    PRODUCT_NOT_IN_CART(HttpStatus.NOT_FOUND, "장바구니에 삭제하려는 상품이 존재하지 않음"),

    //유저 관련
    MEMBER_NOT_FOUND(HttpStatus.NOT_FOUND, "존재하지 않는 유저입니다."),
    //여러가지 관련
    TIME_OUT(HttpStatus.INTERNAL_SERVER_ERROR, "응답 대기시간을 초과하였습니다."),
    INTERRUPT(HttpStatus.INTERNAL_SERVER_ERROR, "인터럽");

    private final HttpStatus httpStatus;
    private final String message;
}
