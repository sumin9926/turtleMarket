package turtleMart.delivery.dto.reqeust;

public record UpdateCourierRequest(
    String name,
    String code,
    String trackingUrlTemplate
) {
}
