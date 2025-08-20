package turtleMart.benchmark;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.redisson.api.RBlockingQueue;
import org.redisson.api.RDelayedQueue;
import org.redisson.api.RLock;
import org.redisson.api.RedissonClient;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Service;
import turtleMart.benchmark.dto.MakeRequest;

import java.util.List;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.TimeUnit;

@Slf4j
@Service
@Profile({"benchmark","improved"})
@RequiredArgsConstructor
public class BmDispatcher {

    private final RedissonClient redisson;
    private final RBlockingQueue<MakeRequest> mainQ;
    private final RDelayedQueue<MakeRequest> delayedQ;
    private final BmServiceBaseline service;

    // 외부 진입점
    public void changePrice(long pocId, int newPrice) {
        enqueue(new MakeRequest("PRICE_CHANGE", pocId, 0, newPrice, 0));
    }

    public void createOrder(List<Long> pocIdList, int quantity) {
        for (long pocId : pocIdList) {
            enqueue(new MakeRequest("ORDER_CREATE", pocId, quantity,0, 0));
        }
    }

    // 즉시 메인큐에 투입
    public void enqueue(MakeRequest req) {
        try {
            boolean ok = mainQ.offer(req);
            if (!ok) {
                // 큐가 가득 찼을 때: 잠시 뒤 재시도
                delayedQ.offer(req, 200, TimeUnit.MILLISECONDS);
                log.warn("mainQ가 가득 참: {}", req);
            }
        } catch (Exception e) {
            log.error("enqueue 실패", e);
            throw e;
        }
    }

    // 워커가 호출하는 진짜 처리기: 락 시도 → 실패면 지연 재시도
    public void processWithLockOrDelay(MakeRequest req) {
        RLock lock = redisson.getLock("lock:poc:" + req.pocId()); // pocId 단위로 Lock 생성
        try {
            // 대기 시간 0ms(락을 잡을 수 있으면 잡고, 못 잡으면 즉시 false 반환), 락 보유 상한 10초(워치독 OFF)
            if (lock.tryLock(0, 10, TimeUnit.SECONDS)) {
                try {
                    handle(req); // 임계영역(실제 가격 변경/주문 생성 로직)
                } finally {
                    if (lock.isHeldByCurrentThread()) lock.unlock();
                }
            } else {
                requeueWithBackoff(req); // 지연 재시도
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new RuntimeException(e);
        }
    }

    private void requeueWithBackoff(MakeRequest req) {
        int next = req.attempt() + 1;
        if (next > 5) { // 상한선 최대 5회
            throw new IllegalStateException("Too many retries: " + req);
        }
        long delayMs = backoffMs(next);
        delayedQ.offer(new MakeRequest(req.type(), req.pocId(), req.quantity(), req.newPrice(), next),
                delayMs, TimeUnit.MILLISECONDS);
    }

    private long backoffMs(int attempt) {
        long base = 100L;
        long exp = (long) (base * Math.pow(2, attempt - 1));
        long jitter = ThreadLocalRandom.current().nextInt(30);
        return Math.min(exp + jitter, 3_000L);
    }

    // 실제 비즈니스 로직
    private void handle(MakeRequest req) {
        switch (req.type()) {
            case "PRICE_CHANGE" -> { service.changePrice(req.pocId(), req.newPrice()); }
            case "ORDER_CREATE" -> { service.createOrder(req.pocId(), req.quantity()); }
            default -> throw new IllegalArgumentException("Unknown type " + req.type());
        }
    }
}
