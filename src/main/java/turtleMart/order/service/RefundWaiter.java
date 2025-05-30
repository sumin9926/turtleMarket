package turtleMart.order.service;

import lombok.RequiredArgsConstructor;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import org.springframework.web.context.request.async.DeferredResult;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;

@Component
@RequiredArgsConstructor
public class RefundWaiter {

    private final RedisTemplate<String, Object> redisTemplate;
    private final Map<Long, DeferredResult<ResponseEntity<Void>>> waitMap = new ConcurrentHashMap<>();

    public DeferredResult<ResponseEntity<Void>> createWaiter(Long orderItemId) {

        DeferredResult<ResponseEntity<Void>> deferredResult = new DeferredResult<>(10_000L); // 10초 뒤 타임아웃
        waitMap.put(orderItemId, deferredResult);

        //Redis에 상태 저장. 기본값 false(환불 미완료 상태). TTL 1분.
        redisTemplate.opsForValue().set(refundKey(orderItemId), false, 1, TimeUnit.MINUTES);

        deferredResult.onTimeout(() -> { // 시간내 응답 없음: 타임 아웃 처리
            waitMap.remove(orderItemId);
            deferredResult.setErrorResult(ResponseEntity.status(HttpStatus.REQUEST_TIMEOUT).build());
        });

        return deferredResult;
    }

    public void completeRefund(Long orderItemId) {
        DeferredResult<ResponseEntity<Void>> deferredResult = waitMap.remove(orderItemId); //처리 완료된 deferred 객체 삭제
        if (deferredResult != null) { // 정상적으로 삭제 되었다면 200 응답
            deferredResult.setResult(ResponseEntity.ok().build());
        }
    }

    private String refundKey(Long orderItemId) {
        return "refund:status:" + orderItemId;
    }
}
