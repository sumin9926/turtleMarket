package turtleMart.review.entity;

import java.util.Arrays;

public enum ReviewReportStatus {
    SUBMITTED, CANCELLED, IN_PROGRESS, COMPLETED;
    //관리자는 상태변경을 IN_PROGRESS, COMPLETED로만 변경 가능, 또한 COMPLETED에서 IN_PROGRESS으로 변경불가하게 제약두기

    public static ReviewReportStatus of(String status) {
        return Arrays.stream(ReviewReportStatus.values())
                .filter(r -> r.name().equalsIgnoreCase(status))
                .findFirst()
                .orElseThrow(() -> new RuntimeException("유효하지 않은 선택입니다"));
    }
}
