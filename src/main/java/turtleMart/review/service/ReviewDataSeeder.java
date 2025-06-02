package turtleMart.review.service;

import lombok.RequiredArgsConstructor;
import org.springframework.boot.CommandLineRunner;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class ReviewDataSeeder implements CommandLineRunner {

    private final OrderItemGenerator orderItemGenerator;
    private final ReviewDummyGenerator generator;

    @Override
    public void run(String... args) {
        orderItemGenerator.generatorOrderItem(30000);
        generator.generate(30000, 100); // 총 100개의 상품에 대해 10,000개의 리뷰 생성
    }
}