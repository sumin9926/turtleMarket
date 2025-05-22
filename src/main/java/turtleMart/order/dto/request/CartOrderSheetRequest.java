package turtleMart.order.dto.request;

import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;

import java.util.Arrays;
import java.util.List;

public record CartOrderSheetRequest(
        @NotNull(message = "상품 ID는 필수입니다.")
        Long productId,
        @NotNull(message = "수량은 필수입니다.")
        @Min(value = 1, message = "수량은 1 이상이어야 합니다.")
        Integer quantity
) {
    public static List<CartOrderSheetRequest> splitItemNameAndQuantity(String items) {
        return Arrays.stream(items.split(","))
                .map(entry -> {
                    String[] parts = entry.split(":");
                    Long productId = Long.parseLong(parts[0]);
                    Integer quantity = Integer.parseInt(parts[1]);
                    return new CartOrderSheetRequest(productId, quantity);
                })
                .toList();
    }
}
