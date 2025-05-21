package turtleMart.delivery.dto.reqeust;

public record CreateCourierRequest(
    String name,
    String code,
    String trackingUrlTemplate
) {
}
