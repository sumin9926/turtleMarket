package turtleMart.review.service;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import turtleMart.review.dto.request.CreateTemplateChoiceRequest;
import turtleMart.review.dto.request.UpdateTemplateChoiceRequest;
import turtleMart.review.dto.response.TemplateChoiceResponse;
import turtleMart.review.entity.*;
import turtleMart.review.repository.ProductReviewTemplateRepository;
import turtleMart.review.repository.TemplateChoiceRepository;

import java.util.ArrayList;
import java.util.List;

@Service
@RequiredArgsConstructor
public class TemplateChoiceService {

    private final TemplateChoiceRepository templateChoiceRepository;
    private final ProductReviewTemplateRepository productReviewTemplateRepository;

    public List<TemplateChoiceResponse> createTemplateChoice(List<CreateTemplateChoiceRequest> requestList, Review review) {
        List<ProductReviewTemplate> productReviewTemplateList = productReviewTemplateRepository
                .findAllByIdDeletedFalse(requestList.stream().map(CreateTemplateChoiceRequest::productReviewTemplateId).toList())
                .stream().toList();

        if (requestList.size() != productReviewTemplateList.size()) {
            throw new RuntimeException("존재하지 않는 상품리뷰 템플릿을 선택하셨습니다");
        }

        List<TemplateChoice> templateChoiceList = new ArrayList<>();

        for (CreateTemplateChoiceRequest request : requestList) {

            ProductReviewTemplate productReviewTemplate = productReviewTemplateRepository.findById(request.productReviewTemplateId())
                    .orElseThrow(() -> new RuntimeException("존재하지 않는 상품리뷰 템플릿입니다"));

            templateChoiceList.add(TemplateChoice.of(review, productReviewTemplate, TemplateChoiceGrade.of(request.answer())));
        }
        templateChoiceRepository.saveAll(templateChoiceList);
        return readTemplateChoice(review.getId());
    }

    public List<TemplateChoiceResponse> readTemplateChoice(Long reviewId) {
        List<TemplateChoice> templateChoiceList = templateChoiceRepository.findByReviewId(reviewId);
        List<TemplateChoiceResponse> choiceResponseList = new ArrayList<>();

        for (TemplateChoice templateChoice : templateChoiceList) {
            ReviewTemplate reviewTemplate = templateChoice.getProductReviewTemplate().getReviewTemplate();

            String question = reviewTemplate.getQuestion();
            String choice = reviewTemplate.getChoice(templateChoice.getChoseAnswer());

            choiceResponseList.add(TemplateChoiceResponse.of(question, choice));
        }

        return choiceResponseList;
    }

    public List<TemplateChoiceResponse> updateTemplateChoice(List<UpdateTemplateChoiceRequest> requestList){
        List<TemplateChoice> templateChoiceList = templateChoiceRepository
                .findAllById(requestList.stream().map(UpdateTemplateChoiceRequest::templateChoiceId).toList())
                .stream().toList();

        if (requestList.size() != templateChoiceList.size()) {
            throw new RuntimeException("존재하지 않는 선택입니다");
        }

        requestList.forEach(r -> {
           TemplateChoice choice = templateChoiceList.stream().filter(c -> c.getId().equals(r.templateChoiceId())).findFirst().get();
           choice.update(TemplateChoiceGrade.of(r.answer()));
       });

        return readTemplateChoice(templateChoiceList.get(0).getReview().getId());
    }

    public void deleteByReviewId(Long reviewId){
        templateChoiceRepository.deleteAllByReviewId(reviewId);
    }
}
