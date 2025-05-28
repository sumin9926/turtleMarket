package turtleMart.product.dto;

import turtleMart.product.entity.RequestOptionGroup;
import turtleMart.product.entity.RequestOptionValue;

import java.util.List;

public record PromoteOptionGroupDto(
        String optionGroupName,
        List<String> optionValueNameList
) {
    public static PromoteOptionGroupDto of(RequestOptionGroup requestOptionGroup, List<RequestOptionValue> requestOptionValues) {
        return new PromoteOptionGroupDto(
                requestOptionGroup.getName(),
                requestOptionValues.stream()
                        .map(RequestOptionValue::getName)
                        .toList()
        );
    }
}
