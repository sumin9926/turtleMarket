package turtleMart.review.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import turtleMart.member.entity.Member;
import turtleMart.member.repository.MemberRepository;
import turtleMart.review.dto.request.CreateReviewReportRequest;
import turtleMart.review.dto.response.ReviewReportResponse;
import turtleMart.review.dto.response.ReviewResponse;
import turtleMart.review.dto.response.TemplateChoiceResponse;
import turtleMart.review.entity.ReasonCode;
import turtleMart.review.entity.Review;
import turtleMart.review.entity.ReviewReport;
import turtleMart.review.entity.ReviewTemplate;
import turtleMart.review.repository.ReasonCodeRepository;
import turtleMart.review.repository.ReviewReportRepository;
import turtleMart.review.repository.ReviewRepository;

import java.util.List;

@Service
@Transactional(readOnly = true)
@RequiredArgsConstructor
public class ReviewReportService {

    private final MemberRepository memberRepository;
    private final ReviewRepository reviewRepository;
    private final ReasonCodeRepository reasonCodeRepository;
    private final ReviewReportRepository reviewReportRepository;
    private final ObjectMapper objectMapper;

    @Transactional
    public ReviewReportResponse createReviewReport(Long memberId, Long reviewId, CreateReviewReportRequest request) throws JsonProcessingException {

        if(!memberRepository.existsById(memberId)){throw new RuntimeException("존재하지 않는 멤버입니다");}
        Member member = memberRepository.getReferenceById(memberId);

        if(!reviewRepository.existsById(reviewId)){throw new RuntimeException("존재하지 않는 리뷰입니다");}
        Review review = reviewRepository.getReferenceById(reviewId);
        List<String> imageUrlList = objectMapper.readValue(review.getImageUrl(), new TypeReference<List<String>>() {});

        if(!reasonCodeRepository.existsById(request.reasonCodeId())){throw new RuntimeException("신고 코드입니다");}
        ReasonCode reasonCode = reasonCodeRepository.getReferenceById(request.reasonCodeId());

        ReviewReport reviewReport = reviewReportRepository.save(ReviewReport.of(review, member, reasonCode, request.ReasonDetail()));

        List<TemplateChoiceResponse> choiceResponseList = review.getTemplateChoiceList().stream()
                .map(t -> {
                    ReviewTemplate reviewTemplate = t.getProductReviewTemplate().getReviewTemplate();
                    return TemplateChoiceResponse.of(reviewTemplate.getQuestion(), reviewTemplate.getChoice(t.getChoseAnswer()));
                })
                .toList();

        return ReviewReportResponse.of(ReviewResponse.of(review, imageUrlList, choiceResponseList), reviewReport);
    }

//    public Page<ReviewReportResponse> readAll(Pageable pageable){
//        Page<ReviewReport> reviewReportPage = reviewReportRepository.findAllPage(pageable);
//
//        return reviewReportPage.map(r -> {
//            Review review = r.getReview();
//            try {
//                List<String> imageUrlList = objectMapper.readValue(review.getImageUrl(), new TypeReference<List<String>>() {});
//            } catch (JsonProcessingException e) {
//                throw new RuntimeException(e);
//            }
//            List<TemplateChoice> templateChoiceList
//            ReviewResponse reviewResponse = ReviewResponse.of(review, )
//                })
//    }

    public ReviewReportResponse readById(Long reviewReportId) throws JsonProcessingException {
        ReviewReport reviewReport = reviewReportRepository.findById(reviewReportId)
                .orElseThrow(() -> new RuntimeException("존재하지 않는 신고건입니다"));

        Review review = reviewReport.getReview();
        List<String> imageUrlList = objectMapper.readValue(review.getImageUrl(), new TypeReference<List<String>>() {});

        List<TemplateChoiceResponse> choiceResponseList = review.getTemplateChoiceList().stream()
                .map(t -> {
                    ReviewTemplate reviewTemplate = t.getProductReviewTemplate().getReviewTemplate();
                    return TemplateChoiceResponse.of(reviewTemplate.getQuestion(), reviewTemplate.getChoice(t.getChoseAnswer()));
                })
                .toList();

        return ReviewReportResponse.of( ReviewResponse.of(review, imageUrlList, choiceResponseList), reviewReport);
    }

    @Transactional
    public ReviewReportResponse updateReviewReport( Long reviewReportId) throws JsonProcessingException {

        ReviewReport reviewReport = reviewReportRepository.findById(reviewReportId)
                .orElseThrow(() -> new RuntimeException("존재하지 않는 신고건입니다"));

        reviewReport.update();

        Review review = reviewReport.getReview();

        List<String> imageUrlList = objectMapper.readValue(review.getImageUrl(), new TypeReference<List<String>>() {});

        List<TemplateChoiceResponse> choiceResponseList = review.getTemplateChoiceList().stream()
                .map(t -> {
                    ReviewTemplate reviewTemplate = t.getProductReviewTemplate().getReviewTemplate();
                    return TemplateChoiceResponse.of(reviewTemplate.getQuestion(), reviewTemplate.getChoice(t.getChoseAnswer()));
                })
                .toList();

        return ReviewReportResponse.of( ReviewResponse.of(review, imageUrlList, choiceResponseList), reviewReport);

    }

    @Transactional
    public void deleteReviewReport(Long reviewReportId){
        if (!reviewReportRepository.existsById(reviewReportId)) {
            throw new RuntimeException("존재하지 않는 신고건입니다");
        }
        reviewReportRepository.deleteById(reviewReportId);
    }

}
