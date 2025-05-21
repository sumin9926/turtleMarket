package turtleMart.delivery.dto.response;

import turtleMart.delivery.entity.Sender;

import java.time.LocalDateTime;

public record UpdateSenderResponse(
    Long id,
    String name,
    String phoneNumber,
    String address,
    String detailAddress,
    LocalDateTime updatedAt
) {
    public static UpdateSenderResponse from(Sender sender) {
        return new UpdateSenderResponse(
            sender.getId(),
            sender.getName(),
            sender.getPhoneNumber(),
            sender.getAddress(),
            sender.getDetailAddress(),
            sender.getUpdatedAt()
        );
    }
}
