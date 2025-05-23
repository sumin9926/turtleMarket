package turtleMart.global.exception;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;

@Getter
@RequiredArgsConstructor
public enum ErrorCode {

    //출고지(물류센터) 관련
    SENDER_NOT_FOUND(HttpStatus.NOT_FOUND, "존재하지 않는 출고지(물류센터)입니다."),
    SENDER_ALREADY_EXISTS(HttpStatus.CONFLICT, "이미 존재하는 출고지(물류센터)입니다."),

    //택배사 관련
    COURIER_NOT_FOUND(HttpStatus.NOT_FOUND, "존재하지 않는 택배사입니다."),
    COURIER_ALREADY_EXISTS(HttpStatus.CONFLICT, "이미 존재하는 택배사입니다."),
    COURIER_DELETE_FAILED(HttpStatus.CONFLICT, "계약된 출고지(물류센터)가 존재하기 때문에 삭제할 수 없습니다."),

    // 배송 관련
    DELIVERY_NOT_FOUND(HttpStatus.NOT_FOUND, "존재하지 않는 배송입니다.");

    private final HttpStatus httpStatus;
    private final String message;
}
