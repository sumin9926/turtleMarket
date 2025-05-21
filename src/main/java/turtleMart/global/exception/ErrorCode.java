package turtleMart.global.exception;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;

@Getter
@RequiredArgsConstructor
public enum ErrorCode {

    //출고지 관련
    NOT_FOUND_SENDER(HttpStatus.NOT_FOUND, "존재하지 않는 출고지(물류센터)입니다.");

    private final HttpStatus httpStatus;
    private final String message;
}
