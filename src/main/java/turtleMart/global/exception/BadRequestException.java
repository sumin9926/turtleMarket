package turtleMart.global.exception;

public class BadRequestException extends CustomRuntimeException {
    public BadRequestException(ErrorCode errorCode) {
        super(errorCode);
    }
}
