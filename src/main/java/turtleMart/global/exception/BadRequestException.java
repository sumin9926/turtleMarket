package turtleMart.global.exception;

public class BadRequestException extends CustomRuntimeException {
    public BadRequestException(ErrorCode errorCode) {
        super(errorCode);
    }

    public BadRequestException(ErrorCode errorCode, String errorMessage) {
        super(errorCode, errorMessage);
    }
}
