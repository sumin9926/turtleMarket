package turtleMart.review.service;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import turtleMart.review.dto.request.CreateReviewTemplateRequest;
import turtleMart.review.dto.response.ReviewTemplateResponse;
import turtleMart.review.entity.ProductReviewTemplate;
import turtleMart.review.entity.ReviewTemplate;
import turtleMart.review.repository.ProductReviewTemplateRepository;
import turtleMart.review.repository.ReviewTemplateRepository;

import java.util.List;

@Service
@RequiredArgsConstructor
public class ReviewTemplateService {

    private final ReviewTemplateRepository reviewTemplateRepository;
    private final ProductReviewTemplateRepository productReviewTemplateRepository;


    public ReviewTemplateResponse createReviewTemplate(CreateReviewTemplateRequest request){

        ReviewTemplate reviewTemplate = ReviewTemplate.of(request.question(), request.low(), request.medium(), request.high());
        reviewTemplateRepository.save(reviewTemplate);

        return ReviewTemplateResponse.from(reviewTemplate);
    }

    public List<ReviewTemplateResponse> readAllReviewTemplate(){
        return reviewTemplateRepository.findAll().stream().map(ReviewTemplateResponse::from).toList();
    }

    public List<ReviewTemplateResponse> readByProductId(Long productId){
        return productReviewTemplateRepository.findByProductId(productId).stream()
                .map(ProductReviewTemplate::getReviewTemplate)
                .map(ReviewTemplateResponse::from)
                .toList();
    }


}
