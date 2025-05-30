package turtleMart.global.redis.config;

import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.connection.RedisConnectionFactory;
import org.springframework.data.redis.listener.ChannelTopic;
import org.springframework.data.redis.listener.PatternTopic;
import org.springframework.data.redis.listener.RedisMessageListenerContainer;
import org.springframework.data.redis.listener.adapter.MessageListenerAdapter;
import turtleMart.global.redis.redisPubSub.RefundRedisSubscriber;
import turtleMart.product.service.PriceChangePubSubListener;

@Configuration
@RequiredArgsConstructor
public class RedisPubSubConfig {

    private final RefundRedisSubscriber refundRedisSubscriber;
    private final RedisConnectionFactory connectionFactory;

    @Bean(name = "refundMessageListenerContainer")// 환불용
    public RedisMessageListenerContainer refundRedisMessageListenerContainer() {
        RedisMessageListenerContainer container = new RedisMessageListenerContainer();
        container.setConnectionFactory(connectionFactory);
        container.addMessageListener(refundRedisSubscriber, new ChannelTopic("refund:completed"));
        return container;
    }

    @Bean(name = "priceChangeListenerContainer")
    public RedisMessageListenerContainer priceChangeRedisMessageListenerContainer(
            RedisConnectionFactory redisConnectionFactory,
            MessageListenerAdapter messageListenerAdapter
    ) {
        RedisMessageListenerContainer redisMessageListenerContainer = new RedisMessageListenerContainer();
        redisMessageListenerContainer.setConnectionFactory(redisConnectionFactory);
        redisMessageListenerContainer.addMessageListener(messageListenerAdapter,new PatternTopic("channel:priceChange"));
        return redisMessageListenerContainer;
    }

    @Bean
    public MessageListenerAdapter messageListenerAdapter(PriceChangePubSubListener priceChangePubSubListener) {
        return new MessageListenerAdapter(priceChangePubSubListener, "onMessage");
    }
}
