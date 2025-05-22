package turtleMart.review.entity;

import java.util.Arrays;

public enum ReviewReportStatus {
    SUBMITTED, CANCELLED, IN_PROGRESS, COMPLETED;

    public static ReviewReportStatus of(String status) {
        return Arrays.stream(ReviewReportStatus.values())
                .filter(r -> r.name().equalsIgnoreCase(status))
                .findFirst()
                .orElseThrow(() -> new RuntimeException("유효하지 않은 선택입니다"));
    }
}
