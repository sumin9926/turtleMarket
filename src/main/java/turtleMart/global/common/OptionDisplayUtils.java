package turtleMart.global.common;

import turtleMart.global.exception.ErrorCode;
import turtleMart.global.exception.NotFoundException;
import turtleMart.product.entity.ProductOptionValue;
import turtleMart.product.repository.ProductOptionValueRepository;

import java.util.Arrays;
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
}
