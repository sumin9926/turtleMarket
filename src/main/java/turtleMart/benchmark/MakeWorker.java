package turtleMart.benchmark;

import jakarta.annotation.PostConstruct;
import jakarta.annotation.PreDestroy;
import org.redisson.api.RBlockingQueue;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;
import turtleMart.benchmark.dto.MakeRequest;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

@Component
@Profile({"benchmark","improved"})
public class MakeWorker {

    private final RBlockingQueue<MakeRequest> mainQ;
    private final BmDispatcher dispatcher;
    private final ExecutorService es = Executors.newSingleThreadExecutor();

    public MakeWorker(RBlockingQueue<MakeRequest> mainQ, BmDispatcher dispatcher) {
        this.mainQ = mainQ;
        this.dispatcher = dispatcher;
    }

    @PostConstruct
    void start() {
        es.submit(() -> {
            while (!Thread.currentThread().isInterrupted()) {
                MakeRequest req = null; // 블로킹
                try {
                    req = mainQ.take();
                } catch (InterruptedException e) {
                    throw new RuntimeException(e);
                }
                dispatcher.processWithLockOrDelay(req);
            }
        });
    }

    @PreDestroy
    void stop() {
        es.shutdownNow();
    }
}
