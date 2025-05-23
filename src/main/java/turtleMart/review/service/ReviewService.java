package turtleMart.review.service;

import com.fasterxml.jackson.core.type.TypeReference;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import turtleMart.global.utill.JsonHelper;
import turtleMart.member.entity.Member;
import turtleMart.member.repository.MemberRepository;
import turtleMart.order.entity.OrderItem;
import turtleMart.order.repository.OrderItemRepository;
import turtleMart.product.entity.Product;
import turtleMart.product.repository.ProductRepository;
import turtleMart.review.dto.request.CreateReviewRequest;
import turtleMart.review.dto.request.CreateTemplateChoiceRequest;
import turtleMart.review.dto.request.UpdateReviewRequest;
import turtleMart.review.dto.request.UpdateTemplateChoiceRequest;
import turtleMart.review.dto.response.ReviewResponse;
import turtleMart.review.dto.response.TemplateChoiceResponse;
import turtleMart.review.entity.*;
import turtleMart.review.repository.ProductReviewTemplateDslRepositoryImpl;
import turtleMart.review.repository.ReviewDslRepositoryImpl;
import turtleMart.review.repository.ReviewRepository;
import java.util.List;

@Service
@Transactional(readOnly = true)
@RequiredArgsConstructor
public class ReviewService {

    private final MemberRepository memberRepository;
    private final ProductRepository productRepository;
    private final OrderItemRepository orderItemRepository;
    private final ReviewRepository reviewRepository;
    private final ProductReviewTemplateDslRepositoryImpl productReviewTemplateDslRepositoryImpl;
    private final ReviewDslRepositoryImpl reviewDslRepositoryImpl;

    @Transactional
    public ReviewResponse createReview(Long memberId, Long productId, CreateReviewRequest request) {

        if (!memberRepository.existsById(memberId)) {throw new RuntimeException("존재하지 않는 회원입니다");}
        Member member = memberRepository.getReferenceById(memberId);

        if (!productRepository.existsById(productId)) {throw new RuntimeException("존재하지 않는 상품입니다");}
        Product product = productRepository.getReferenceById(productId);

        if (!orderItemRepository.existsById(request.orderItemId())) {throw new RuntimeException("존재하지 않는 주문상품입니다");}
        OrderItem orderItem = orderItemRepository.getReferenceById(request.orderItemId());

        //주문상품의 상태가 배송완료인지를 확인하는 로직 추가 예정

        if (reviewRepository.existsByOrderItemId(orderItem.getId())) {throw new RuntimeException("주문건에 대한 리뷰는 한번만 작성가능합니다");}

        String dbImageList = JsonHelper.toJson(request.imageUrlList());
        Review review = Review.of(member, product, orderItem, request.title(), request.content(), request.rating(), dbImageList);

        List<ProductReviewTemplate> productReviewTemplateList = productReviewTemplateDslRepositoryImpl.findByIdInWithReviewTemplate(
                request.templateChoiceList().stream().map(CreateTemplateChoiceRequest::productReviewTemplateId).toList());

        request.templateChoiceList().forEach(c -> {
            Long targetId = c.productReviewTemplateId();
            ProductReviewTemplate matchedTemplate = productReviewTemplateList.stream()
                    .filter(p -> p.getId().equals(targetId))
                    .findFirst()
                    .orElseThrow(() -> new RuntimeException("존재하지 않는 상품 리뷰 템플릿입니다"));

            TemplateChoice templateChoice = TemplateChoice.of(matchedTemplate, TemplateChoiceGrade.of(c.answer()));
            templateChoice.setReview(review);
        });

        reviewRepository.save(review);

        List<TemplateChoiceResponse> choiceResponseList = readTemplateChoiceByReview(review);
        return ReviewResponse.of(review, request.imageUrlList(), choiceResponseList);
    }

    public ReviewResponse readReview(Long reviewId) {

        Review review = findByIdElseThrow(reviewId);

        List<String> imageUrlList = JsonHelper.fromJsonToList(review.getImageUrl(), new TypeReference<>() {});
        List<TemplateChoiceResponse> choiceResponseList = readTemplateChoiceByReview(review);

        return ReviewResponse.of(review, imageUrlList, choiceResponseList);
    }


    @Transactional
    public ReviewResponse updateReview(Long memberId, Long reviewId, UpdateReviewRequest request) {

        Member member = memberRepository
                .findById(memberId).orElseThrow(() -> new RuntimeException("존재하지 않는 회원입니다"));

        Review review = findByIdElseThrow(reviewId);

        if (!member.getId().equals(review.getMember().getId())) {throw new RuntimeException("본인이 작성한 리뷰만 수정가능합니다");}

        String dbImageList = JsonHelper.toJson(request.imageUrlList());
        review.update(request.title(), request.content(), request.rating(), dbImageList);

        List<TemplateChoiceResponse> choiceResponseList = review.getTemplateChoiceList().stream()
                .map(c -> {
                    UpdateTemplateChoiceRequest updateRequest = request.templateChoiceList().stream()
                            .filter(r -> r.templateChoiceId().equals(c.getId()))
                            .findFirst().orElseThrow(() -> new RuntimeException("존재하지 않는 상품 리뷰 템플릿입니다"));

                    c.update(TemplateChoiceGrade.of(updateRequest.answer()));
                    ReviewTemplate reviewTemplate = c.getProductReviewTemplate().getReviewTemplate();
                    return TemplateChoiceResponse.of(reviewTemplate.getQuestion(), reviewTemplate.getChoice(c.getChoseAnswer()));
                })
                .toList();

        return ReviewResponse.of(review, request.imageUrlList(), choiceResponseList);
    }

    @Transactional
    public void deleteReview(Long memberId, Long reviewId) {

        if (!memberRepository.existsById(memberId)) {throw new RuntimeException("존재하지 않는 회원입니다");}
        Member member = memberRepository.getReferenceById(memberId);

        if (!reviewRepository.existsById(reviewId)) {throw new RuntimeException("존재하지 않는 리뷰입니다");}
        Review review = reviewRepository.getReferenceById(reviewId);

        if (!member.getId().equals(review.getMember().getId())) {throw new RuntimeException("본인이 작성한 리뷰만 삭제가능합니다");}

        review.delete();
    }

    private Review findByIdElseThrow(Long reviewId) {

        return reviewDslRepositoryImpl.findByIdWithChoice(reviewId)
                .orElseThrow(() -> new RuntimeException("존재하지 않는 리뷰입니다"));// 메서드 추출
    }

    //어디로 뺄지 고민중
    private List<TemplateChoiceResponse> readTemplateChoiceByReview(Review review){

        return review.getTemplateChoiceList().stream()
                .map(t -> {
                    ReviewTemplate reviewTemplate = t.getProductReviewTemplate().getReviewTemplate();
                    return TemplateChoiceResponse.of(reviewTemplate.getQuestion(), reviewTemplate.getChoice(t.getChoseAnswer()));
                })
                .toList();
    }
}
