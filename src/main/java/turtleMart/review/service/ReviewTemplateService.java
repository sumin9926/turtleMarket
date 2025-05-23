package turtleMart.review.service;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import turtleMart.review.dto.request.CreateReviewTemplateRequest;
import turtleMart.review.dto.request.UpdateReviewTemplateRequest;
import turtleMart.review.dto.response.ReviewTemplateResponse;
import turtleMart.review.entity.ProductReviewTemplate;
import turtleMart.review.entity.ReviewTemplate;
import turtleMart.review.repository.ProductReviewTemplateRepository;
import turtleMart.review.repository.ReviewTemplateRepository;
import java.util.List;

@Service
@Transactional(readOnly = true)
@RequiredArgsConstructor
public class ReviewTemplateService {

    private final ReviewTemplateRepository reviewTemplateRepository;
    private final ProductReviewTemplateRepository productReviewTemplateRepository;


    @Transactional
    public ReviewTemplateResponse createReviewTemplate(CreateReviewTemplateRequest request){
        ReviewTemplate reviewTemplate = ReviewTemplate.of(
                request.question(),
                request.satisfaction_low(),
                request.satisfaction_medium(),
                request.satisfaction_high()
        );
        reviewTemplateRepository.save(reviewTemplate);
        return ReviewTemplateResponse.from(reviewTemplate);
    }

    public List<ReviewTemplateResponse> readAllReviewTemplate(){
        return reviewTemplateRepository.findAllDeletedFalse().stream().map(ReviewTemplateResponse::from).toList();
    }

    public List<ReviewTemplateResponse> readByProductId(Long productId){
        return productReviewTemplateRepository.findByProductId(productId).stream()
                .map(ProductReviewTemplate::getReviewTemplate)
                .map(ReviewTemplateResponse::from)
                .toList();
    }

    @Transactional
    public ReviewTemplateResponse updateReviewTemplate(UpdateReviewTemplateRequest request, Long reviewTemplateId){
        ReviewTemplate reviewTemplate = findByIdElseThrow(reviewTemplateId);
        if(reviewTemplate.isDeleted()){throw new RuntimeException("삭제된 리뷰 템플릿입니다");}

        reviewTemplate.update(request.question(), request.satisfaction_low(), request.satisfaction_medium(), request.high());
        return ReviewTemplateResponse.from(reviewTemplate);
    }

    @Transactional
    public void deleteReviewTemplate(Long reviewTemplateId){
        ReviewTemplate reviewTemplate = findByIdElseThrow(reviewTemplateId);

        if(reviewTemplate.isDeleted()){throw new RuntimeException("삭제된 리뷰 템플릿입니다");}
        reviewTemplate.delete();
    }

    private ReviewTemplate findByIdElseThrow(Long reviewTemplateId){
      return reviewTemplateRepository.findByIdIsDeletedFalse(reviewTemplateId)
                .orElseThrow(() -> new RuntimeException("존재하지 않는 리뷰 템플릿입니다"));
    }
}
