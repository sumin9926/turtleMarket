package turtleMart.global.common;

import org.redisson.api.RBlockingQueue;
import org.redisson.api.RDelayedQueue;
import org.redisson.api.RedissonClient;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import turtleMart.benchmark.dto.MakeRequest;

@Configuration
@Profile({"benchmark","improved"})
public class QueueConfig {

    public static final String MAKE_QUEUE_KEY = "bm:order.make";

    @Bean
    public RBlockingQueue<MakeRequest> mainQueue(RedissonClient redisson) {
        return redisson.getBlockingQueue(MAKE_QUEUE_KEY);
    }

    @Bean(destroyMethod = "destroy") // 종료 시 지연큐 스케줄 키 정리
    public RDelayedQueue<MakeRequest> delayedQueue(RBlockingQueue<MakeRequest> mainQ, RedissonClient redisson) {
        return redisson.getDelayedQueue(mainQ);
    }
}
