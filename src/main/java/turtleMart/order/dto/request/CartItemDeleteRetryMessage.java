package turtleMart.order.dto.request;

public record CartItemDeleteRetryMessage(
        String cartItemId,
        String key,
        Integer attempt //재시도 회차
) {
    public static CartItemDeleteRetryMessage incrementAttempt(CartItemDeleteRetryMessage message){
        return new CartItemDeleteRetryMessage(message.cartItemId, message.key, message.attempt+1);
    }
}
