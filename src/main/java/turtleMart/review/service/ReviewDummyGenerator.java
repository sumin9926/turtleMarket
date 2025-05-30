package turtleMart.review.service;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import turtleMart.member.entity.Member;
import turtleMart.member.repository.MemberRepository;
import turtleMart.order.entity.OrderItem;
import turtleMart.order.repository.OrderItemRepository;
import turtleMart.product.entity.Product;
import turtleMart.review.entity.Review;
import turtleMart.review.repository.ReviewRepository;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

@Component
@RequiredArgsConstructor
public class ReviewDummyGenerator {

    private final ReviewRepository reviewRepository;
    private final MemberRepository memberRepository;
//    private final Faker faker = new Faker(new Locale("ko")); // 한글 Faker
    private final Random random = new Random();

    private final String[] titles = {
            "완전 만족해요", "배송 빠르고 좋아요", "별로였어요", "재구매 의사 있습니다", "가격 대비 괜찮아요",
            "기대 이상이에요", "딱 보통", "품질이 너무 별로", "아주 만족합니다", "실망이에요"
    };


    private final String[] templates = {
            "배송이 %s고 품질이 %s네요. %s!",
            "포장도 %s고 제품이 %s합니다. %s.",
            "%s 가격에 %s 퀄리티. %s.",
            "%s해서 쓰기 좋아요. 특히 %s 기능이 마음에 들어요.",
            "생각보다 %s네요. %s.",
            "%s %s %s 다시 살래요."
    };

    private final String[] words1 = {"빠르", "느리", "좋", "별로", "맛", "향", "양", "크기", "실용"};
    private final String[] words2 = {"좋", "별로", "괜찮", "훌륭", "나쁨", "애매함", "그다지", "최악", "만족"};
    private final String[] endings = {"재구매 의사 있어요", "추천합니다", "별로에요", "또 이용할게요", "다신 안살래요"};
    private final Integer[] ratingList = {1,2,3,4,5};

    private final OrderItemRepository orderItemRepository;

    @Transactional
    public void generate(int count, int productCount) {


        Member member = memberRepository.getReferenceById(1L); // 동일한 회원 사용
        List<OrderItem> orderItems = orderItemRepository.findAll(); // 여러 주문상품 가져오기

        List<Review> reviews = new ArrayList<>();

        int reviewCount = Math.min(count, orderItems.size()); // 최대 count개 또는 주문상품 수만큼

        for (int i = 0; i < reviewCount; i++) {
            OrderItem orderItem = orderItems.get(i);
            Product product = orderItem.getProductOptionCombination().getProduct(); // 주문상품에서 상품 참조

            Review review = Review.of(
                    member,
                    product,
                    orderItem,
                    pick(titles),
                    makeContent(),
                    pick(ratingList),
                    ""
            );

            reviews.add(review);
        }

        reviewRepository.saveAll(reviews);
    }

    private String makeContent() {
        String template = templates[random.nextInt(templates.length)];
        return String.format(template,
                pick(words1),
                pick(words2),
                pick(endings)
        );
    }

    private <T> T pick(T[] arr) {
        return arr[random.nextInt(arr.length)];
    }
}
@Component
class OrderItemGenerator{

    public void generatorOrderItem(){

    }

}
