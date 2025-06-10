package turtleMart.global.common;

import turtleMart.global.exception.ErrorCode;
import turtleMart.global.exception.NotFoundException;
import turtleMart.product.entity.ProductOptionValue;
import turtleMart.product.repository.ProductOptionValueRepository;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

/*OptionCombination 의 uniqueKey를 사용자가 읽기 좋은 문자열(ex)'레드/M')로 만드는 유틸*/
public class OptionDisplayUtils {

    public static String buildOptionDisplay(String uniqueKey, ProductOptionValueRepository repository) {
        return Arrays.stream(uniqueKey.split("-"))
                .map(Long::valueOf)
                .map(id -> repository.findById(id)
                        .orElseThrow(() -> new NotFoundException(ErrorCode.PRODUCT_OPTION_VALUE_NOT_FOUND)))
                .map(ProductOptionValue::getName)
                .collect(Collectors.joining(" / "));
    }

    public static Map<String, String> buildOptionDisplayMap(Set<String> uniqueKeySet, ProductOptionValueRepository repository) {

        Set<Long> allOptionIdSet = uniqueKeySet.stream()
                .flatMap(k -> Arrays.stream(k.split("-")))
                .map(Long::valueOf)
                .collect(Collectors.toSet());

        Map<Long, ProductOptionValue> optionMap = repository.findAllById(allOptionIdSet)
                .stream()
                .collect(Collectors.toMap(ProductOptionValue::getId, Function.identity()));

        // 각 uniqueKey 에 상응하는 옵션 문자열(ex)'레드/M') 만들기
        Map<String, String> result = new HashMap<>();
        for (String key : uniqueKeySet) {
            String display = Arrays.stream(key.split("-"))
                    .map(Long::valueOf)
                    .map(id -> Optional.ofNullable(optionMap.get(id))
                            .orElseThrow(() -> new NotFoundException(ErrorCode.PRODUCT_OPTION_VALUE_NOT_FOUND)))
                    .map(ProductOptionValue::getName)
                    .collect(Collectors.joining(" / "));
            result.put(key, display);
        }

        return result;
    }
}
