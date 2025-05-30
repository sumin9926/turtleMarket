package turtleMart.global.exception;

public class ConflictRequestException extends CustomRuntimeException {
    public ConflictRequestException(ErrorCode errorCode) {
        super(errorCode);
    }
}
