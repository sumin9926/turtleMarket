package turtleMart.global.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.kafka.core.DefaultKafkaProducerFactory;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.kafka.core.ProducerFactory;

import java.util.HashMap;

@Configuration
public class KafkaProducerConfig {

    @Bean
    public ProducerFactory<String, Object> producerFactoryForObject() {
        return new DefaultKafkaProducerFactory<>(new HashMap<>());
    }

    @Bean
    public ProducerFactory<String, String> producerFactoryForString() {
        return new DefaultKafkaProducerFactory<>(new HashMap<>());
    }

    @Bean
    public KafkaTemplate<String, Object> objectKafkaTemplate() {
        return new KafkaTemplate<>(producerFactoryForObject());
    }

    @Bean
    public KafkaTemplate<String, String> stringKafkaTemplate() {
        return new KafkaTemplate<>(producerFactoryForString());
    }
}
