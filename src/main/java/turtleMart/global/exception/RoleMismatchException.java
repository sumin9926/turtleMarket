package turtleMart.global.exception;

public class RoleMismatchException extends CustomRuntimeException {
    public RoleMismatchException(ErrorCode errorCode) {super(errorCode);
    }
}
