package turtleMart.review.service;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import turtleMart.review.dto.request.CreateReviewTemplateRequest;
import turtleMart.review.dto.response.ReviewTemplateResponse;
import turtleMart.review.entity.ReviewTemplate;
import turtleMart.review.repository.ReviewTemplateRepository;

@Service
@RequiredArgsConstructor
public class ReviewTemplateService {

    private final ReviewTemplateRepository reviewTemplateRepository;


    public ReviewTemplateResponse createReviewTemplate(CreateReviewTemplateRequest request){

        ReviewTemplate reviewTemplate = ReviewTemplate.of(request.question(), request.bad(), request.normal(), request.good());
        reviewTemplateRepository.save(reviewTemplate);

        return ReviewTemplateResponse.from(reviewTemplate);
    }


}
