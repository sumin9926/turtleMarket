package turtleMart.review.service;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import turtleMart.review.dto.request.CreateTemplateChoiceRequest;
import turtleMart.review.dto.response.TemplateChoiceResponse;
import turtleMart.review.entity.ProductReviewTemplate;
import turtleMart.review.entity.Review;
import turtleMart.review.entity.TemplateChoice;
import turtleMart.review.entity.TemplateChoiceGrade;
import turtleMart.review.repository.ProductReviewTemplateRepository;
import turtleMart.review.repository.TemplateChoiceRepository;

import java.util.ArrayList;
import java.util.List;

@Service
@RequiredArgsConstructor
public class TemplateChoiceService {

    private final TemplateChoiceRepository templateChoiceRepository;
    private final ProductReviewTemplateRepository productReviewTemplateRepository;

    public List<TemplateChoiceResponse> createTemplateChoice(List<CreateTemplateChoiceRequest> requestList, Review review){
        List<ProductReviewTemplate> productReviewTemplateList = productReviewTemplateRepository.findAllByIdDeletedFalse(requestList.stream()
                .map(CreateTemplateChoiceRequest::productReviewTemplateId).toList())
                .stream().toList();

        if(requestList.size() != productReviewTemplateList.size()){throw new RuntimeException("존재하지 않는 리뷰 템플릿을 선택하셨습니다");}

        List<TemplateChoice> templateChoiceList = new ArrayList<>();

        for(CreateTemplateChoiceRequest request : requestList ){

            ProductReviewTemplate productReviewTemplate = productReviewTemplateRepository.findById(request.productReviewTemplateId())
                            .orElseThrow(() -> new RuntimeException("존재하지 않는 상품리뷰 템플릿입니다"));

            templateChoiceList.add(TemplateChoice.of(review, productReviewTemplate, TemplateChoiceGrade.of(request.answer())));
        }
        templateChoiceRepository.saveAll(templateChoiceList);
        return null;
    }
}
