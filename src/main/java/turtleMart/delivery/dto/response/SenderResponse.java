package turtleMart.delivery.dto.response;

import turtleMart.delivery.entity.Courier;
import turtleMart.delivery.entity.Sender;

import java.time.LocalDateTime;

public record SenderResponse(
    Long id,
    String name,
    String phoneNumber,
    String address,
    String detailAddress,
    CourierInfo courier,
    LocalDateTime createdAt
) {
    public static SenderResponse from(Sender sender) {
        return new SenderResponse(
            sender.getId(),
            sender.getName(),
            sender.getPhoneNumber(),
            sender.getAddress(),
            sender.getDetailAddress(),
            CourierInfo.from(sender.getCourier()),
            sender.getCreatedAt()
        );
    }

    public record CourierInfo(
        Long id,
        String name,
        String code
    ) {
        public static CourierInfo from(Courier courier) {
            return new CourierInfo(courier.getId(), courier.getName(), courier.getCode());
        }
    }
}
