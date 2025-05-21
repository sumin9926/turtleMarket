package turtleMart.global.common;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import turtleMart.global.exception.ErrorCode;

@Getter
@RequiredArgsConstructor
public class ErrorResponse {

    private final HttpStatus httpStatus;
    private final String errorMessage;

    public ErrorResponse(ErrorCode errorCode) {
        this.httpStatus = errorCode.getHttpStatus();
        this.errorMessage = errorCode.getMessage();
    }

    public static ErrorResponse of(ErrorCode errorCode) {
        return new ErrorResponse(errorCode.getHttpStatus(), errorCode.getMessage());
    }
}
