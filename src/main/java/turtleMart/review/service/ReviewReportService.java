package turtleMart.review.service;

import com.fasterxml.jackson.core.JacksonException;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import turtleMart.member.entity.Member;
import turtleMart.member.repository.MemberRepository;
import turtleMart.review.dto.request.CreateReviewReportRequest;
import turtleMart.review.dto.response.ReviewReportResponse;
import turtleMart.review.dto.response.ReviewResponse;
import turtleMart.review.dto.response.TemplateChoiceResponse;
import turtleMart.review.entity.*;
import turtleMart.review.repository.ReasonCodeRepository;
import turtleMart.review.repository.ReviewReportRepository;
import turtleMart.review.repository.ReviewRepository;

import java.util.List;

@Service
@RequiredArgsConstructor
public class ReviewReportService {

    private final MemberRepository memberRepository;
    private final ReviewRepository reviewRepository;
    private final ReasonCodeRepository reasonCodeRepository;
    private final ReviewReportRepository reviewReportRepository;
    private final ObjectMapper objectMapper;

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

        return ReviewReportResponse.of( ReviewResponse.of(review, imageUrlList, choiceResponseList), reviewReport);
    }
}
