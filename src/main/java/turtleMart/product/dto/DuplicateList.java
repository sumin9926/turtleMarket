package turtleMart.product.dto;

import java.util.List;

public record DuplicateList(
        String message,
        List<String> valueCombination
) {
    public static DuplicateList from(List<String> valueCombination) {
        return new DuplicateList("이미 등록된 조합목록", valueCombination);
    }
}
