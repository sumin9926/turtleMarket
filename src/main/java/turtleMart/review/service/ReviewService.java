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
import turtleMart.review.repository.ProductReviewTemplateRepository;
import turtleMart.review.repository.ReviewRepository;
import java.util.ArrayList;
import java.util.List;

@Service
@Transactional(readOnly = true)
@RequiredArgsConstructor
public class ReviewService {

    private final MemberRepository memberRepository;
    private final ProductRepository productRepository;
    private final OrderItemRepository orderItemRepository;
    private final ReviewRepository reviewRepository;
    private final ProductReviewTemplateRepository productReviewTemplateRepository;

    @Transactional
    public ReviewResponse createReview(Long memberId, Long productId, CreateReviewRequest request){
        if(!memberRepository.existsById(memberId)){throw  new RuntimeException("존재하지 않는 회원입니다");}
        Member member = memberRepository.getReferenceById(memberId);

        if(!productRepository.existsById(productId)){throw  new RuntimeException("존재하지 않는 상품입니다");}
        Product product = productRepository.getReferenceById(productId);

        if(!orderItemRepository.existsById(request.orderItemId())){throw  new RuntimeException("존재하지 않는 주문상품입니다");}
        OrderItem orderItem = orderItemRepository.getReferenceById(request.orderItemId());

        //주문상품의 상태가 배송완료인지를 확인하는 로직 추가 예정

        if (reviewRepository.existsByOrderItemId(orderItem.getId())) {
            throw new RuntimeException("주문건에 대한 리뷰는 한번만 작성가능합니다");
        }

        String dbImageList = JsonHelper.toJson(request.imageUrlList());

        Review review = Review.of(
                member,
                product,
                orderItem,
                request.title(),
                request.content(),
                request.rating(),
                dbImageList,
                new ArrayList<>()
        );

        List<TemplateChoice> templateChoiceList = new ArrayList<>();

        for (CreateTemplateChoiceRequest choiceRequest : request.templateChoiceList()) {

            ProductReviewTemplate productReviewTemplate = productReviewTemplateRepository.findById(choiceRequest.productReviewTemplateId())
                    .orElseThrow(() -> new RuntimeException("존재하지 않는 상품 리뷰 템플릿입니다"));
            templateChoiceList.add(TemplateChoice.of(review, productReviewTemplate, TemplateChoiceGrade.of(choiceRequest.answer())));
        }

        templateChoiceList.forEach(t -> t.setReview(review));
        reviewRepository.save(review);

        List<TemplateChoiceResponse> choiceResponseList =
                review.getTemplateChoiceList().stream()
                        .map(r -> {
                            ReviewTemplate reviewTemplate = r.getProductReviewTemplate().getReviewTemplate();
                            return TemplateChoiceResponse.of(reviewTemplate.getQuestion(), reviewTemplate.getChoice(r.getChoseAnswer()));
                        })
                        .toList();

        return ReviewResponse.of(review, request.imageUrlList(), choiceResponseList);
    }

    public ReviewResponse readReview(Long reviewId) {
        Review review = reviewRepository.findById(reviewId)
                .orElseThrow(() -> new RuntimeException("존재하지 않는 리뷰입니다"));// 메서드 추출

        List<String> imageUrlList = JsonHelper.fromJsonToList(review.getImageUrl(), new TypeReference<>(){});
        List<TemplateChoiceResponse> choiceResponseList = review.getTemplateChoiceList().stream()
                .map(t -> {
                    ReviewTemplate reviewTemplate = t.getProductReviewTemplate().getReviewTemplate();
                    return TemplateChoiceResponse.of(reviewTemplate.getQuestion(), reviewTemplate.getChoice(t.getChoseAnswer()));
                        })
                .toList();

        return ReviewResponse.of(review, imageUrlList, choiceResponseList);
    }

//    public Page<ReviewResponse> readByMemberId(Long memberId, Pageable pageable){
//        Page<Review> reviewPage = templateChoiceRepository.findByMemberId(memberId, pageable).map(TemplateChoice::getReview);
//        templateChoiceService.readTemplateChoice(reviewPage.stream().map(Review::getId));
//    }

    @Transactional
    public ReviewResponse updateReview(Long memberId, Long reviewId, UpdateReviewRequest request) {
        Member member = memberRepository
                .findById(memberId).orElseThrow(() -> new RuntimeException("존재하지 않는 회원입니다"));

        Review review = reviewRepository.findById(reviewId)
                .orElseThrow(() -> new RuntimeException("존재하지 않는 리뷰입니다"));

        if (!member.getId().equals(review.getMember().getId())) {
            throw new RuntimeException("본인이 작성한 리뷰만 수정가능합니다");
        }

        String dbImageList = JsonHelper.toJson(request.imageUrlList());

        review.update(request, dbImageList);
        List<TemplateChoiceResponse> choiceResponseList = review.getTemplateChoiceList().stream()
                .map(c -> {
                    UpdateTemplateChoiceRequest updateRequest = request.templateChoiceList().stream()
                            .filter(r -> r.templateChoiceId().equals(c.getId()))
                            .findFirst().get();

                    c.update(TemplateChoiceGrade.of(updateRequest.answer()));
                    ReviewTemplate reviewTemplate = c.getProductReviewTemplate().getReviewTemplate();
                    return TemplateChoiceResponse.of(reviewTemplate.getQuestion(), reviewTemplate.getChoice(c.getChoseAnswer()));

                })
                .toList();

        return ReviewResponse.of(review, request.imageUrlList(), choiceResponseList);
    }

    @Transactional
    public void deleteReview(Long memberId, Long reviewId) {
        if(!memberRepository.existsById(memberId)){throw  new RuntimeException("존재하지 않는 회원입니다");}
        Member member = memberRepository.getReferenceById(memberId);

        if(!reviewRepository.existsById(reviewId)){throw  new RuntimeException("존재하지 않는 리뷰입니다");}
        Review review = reviewRepository.getReferenceById(reviewId);

        if (!member.getId().equals(review.getMember().getId())) {
            throw new RuntimeException("본인이 작성한 리뷰만 삭제가능합니다");
        }
        reviewRepository.deleteById(reviewId);
    }
}
