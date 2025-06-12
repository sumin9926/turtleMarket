package turtleMart.order.service;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import org.springframework.web.context.request.async.DeferredResult;
import turtleMart.global.exception.BadRequestException;
import turtleMart.global.exception.ErrorCode;
import turtleMart.order.dto.request.OrderWrapperRequest;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

@Component
@RequiredArgsConstructor
public class OrderWaiter {

    private final Map<String, DeferredResult<ResponseEntity<OrderWrapperRequest>>> orderWaitMap = new ConcurrentHashMap<>();
    private final static Long TIMEOUT_VALUE = 10_000L; // 10초 뒤 타임아웃

    public DeferredResult<ResponseEntity<OrderWrapperRequest>> createWaiter(String orderKey){

        DeferredResult<ResponseEntity<OrderWrapperRequest>> deferredResult = new DeferredResult<>(TIMEOUT_VALUE);

        if(null != orderWaitMap.putIfAbsent(orderKey, deferredResult)){
            throw new BadRequestException(ErrorCode.DUPLICATE_ORDER_REQUEST);
        }

        deferredResult.onTimeout(() -> { // 시간내 응답 없음: 타임 아웃 처리
            orderWaitMap.remove(orderKey);
            deferredResult.setErrorResult(ResponseEntity.status(HttpStatus.REQUEST_TIMEOUT).build());
        });

        return deferredResult;
    }
   // 10초 뒤 타임아웃과 컴플릿 오더의 시간이 겹친다면?,,
    public void completeOrder(String key, OrderWrapperRequest response) {
        DeferredResult<ResponseEntity<OrderWrapperRequest>> result = orderWaitMap.remove(key);
        if (result != null) {
            result.setResult(ResponseEntity.status(HttpStatus.CREATED).body(response));
        }
    }
}
