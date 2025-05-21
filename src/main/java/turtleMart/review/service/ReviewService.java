package turtleMart.review.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import turtleMart.member.entity.Member;
import turtleMart.member.repository.MemberRepository;
import turtleMart.order.entity.OrderItem;
import turtleMart.order.repository.OrderItemRepository;
import turtleMart.product.entity.Product;
import turtleMart.product.repository.ProductRepository;
import turtleMart.review.dto.request.CreateReviewRequest;
import turtleMart.review.dto.response.ReviewResponse;
import turtleMart.review.dto.response.TemplateChoiceResponse;
import turtleMart.review.entity.Review;
import turtleMart.review.repository.ReviewRepository;

import java.util.List;

@Service
@RequiredArgsConstructor
public class ReviewService {

    private final MemberRepository memberRepository;
    private final ProductRepository productRepository;
    private final OrderItemRepository orderItemRepository;
    private final ReviewRepository reviewRepository;
    private final ObjectMapper objectMapper;
    private final TemplateChoiceService templateChoiceService;

    public ReviewResponse createReview(Long memberId, Long productId, CreateReviewRequest request) throws JsonProcessingException {
        Member member = memberRepository
                .findById(memberId).orElseThrow(() -> new RuntimeException("존재하지 않는 회원입니다"));
        Product product = productRepository.findById(productId)
                .orElseThrow(() -> new RuntimeException("존재하지 않는 상품입니다"));
        OrderItem orderItem = orderItemRepository.findById(request.orderItemId())
                .orElseThrow(() -> new RuntimeException("존재하지 않는 주문상품입니다"));

        //주문상품의 상태를 확인하는 로직 추가 예정

        if (reviewRepository.existsByOrderItemId(orderItem.getId())) {
            throw new RuntimeException("주문건에 대한 리뷰는 한번만 작성가능합니다");
        }

        String dbImageList = objectMapper.writeValueAsString(request.imageUrlList());
        Review review = Review.of(
                member,
                product,
                orderItem,
                request.title(),
                request.content(),
                request.rating(),
                dbImageList
        );
        reviewRepository.save(review);

        List<TemplateChoiceResponse> choiceResponseList =
                templateChoiceService.createTemplateChoice(request.templateChoiceList(), review);

        return ReviewResponse.of(review, request.imageUrlList(), choiceResponseList);
    }
}
