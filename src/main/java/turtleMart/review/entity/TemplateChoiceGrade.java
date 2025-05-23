package turtleMart.review.entity;

import java.util.Arrays;

public enum TemplateChoiceGrade {
    LOW,
    MEDIUM,
    HIGH;

    public static TemplateChoiceGrade of(String choice) {
        return Arrays.stream(TemplateChoiceGrade.values())
                .filter(r -> r.name().equalsIgnoreCase(choice))
                .findFirst()
                .orElseThrow(() -> new RuntimeException("유효하지 않은 선택입니다"));
    }
}
