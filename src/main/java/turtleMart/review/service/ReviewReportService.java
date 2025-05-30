package turtleMart.review.service;

import com.fasterxml.jackson.core.type.TypeReference;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import turtleMart.global.exception.BadRequestException;
import turtleMart.global.exception.ErrorCode;
import turtleMart.global.exception.NotFoundException;
import turtleMart.global.utill.JsonHelper;
import turtleMart.member.entity.Member;
import turtleMart.member.repository.MemberRepository;
import turtleMart.review.dto.request.CancelReviewReportRequest;
import turtleMart.review.dto.request.CreateReviewReportRequest;
import turtleMart.review.dto.request.UpdateReviewReportStatusRequest;
import turtleMart.review.dto.response.ReviewReportResponse;
import turtleMart.review.dto.response.TemplateChoiceResponse;
import turtleMart.review.entity.*;
import turtleMart.review.repository.*;
import java.util.List;

@Service
@Transactional(readOnly = true)
@RequiredArgsConstructor
public class ReviewReportService {

    private final MemberRepository memberRepository;
    private final ReasonCodeRepository reasonCodeRepository;
    private final ReviewReportRepository reviewReportRepository;
    private final ReviewReportDslRepositoryImpl reviewReportDslRepository;
    private final ReviewDslRepositoryImpl reviewDslRepository;


    @Transactional
    public ReviewReportResponse createReviewReport(Long memberId, Long reviewId, CreateReviewReportRequest request){

        if(!memberRepository.existsById(memberId)){throw new NotFoundException(ErrorCode.MEMBER_NOT_FOUND);}
        Member member = memberRepository.getReferenceById(memberId);

        Review review = reviewDslRepository.findByIdWithChoice(reviewId)
                                .orElseThrow(() -> new NotFoundException(ErrorCode.REVIEW_NOT_FOUND));

        if(reviewReportRepository.existsByMemberIdAndReviewId(member.getId(), review.getId())){
            throw new BadRequestException(ErrorCode.DUPLICATE_REVIEW_REPORT);
        }

        if(!reasonCodeRepository.existsById(request.reasonCodeId())){throw new NotFoundException(ErrorCode.REASON_CODE_NOT_FOUND);}
        ReasonCode reasonCode = reasonCodeRepository.getReferenceById(request.reasonCodeId());

        ReviewReport reviewReport = reviewReportRepository.save(ReviewReport.of(review, member, reasonCode, request.ReasonDetail()));

        List<TemplateChoiceResponse> choiceResponseList = readTemplateChoiceByReview(review);
        List<String> imageUrlList = JsonHelper.fromJsonToList(review.getImageUrl(), new TypeReference<>() {});
        return ReviewReportResponse.of(review, imageUrlList, choiceResponseList, reviewReport);
    }


    public ReviewReportResponse readById(Long reviewReportId){
        ReviewReport reviewReport = reviewReportDslRepository.findByIdWithReportCode(reviewReportId)
                .orElseThrow(() -> new NotFoundException(ErrorCode.REVIEW_REPORT_NOT_FOUND));

        Review review = reviewReport.getReview();
        List<String> imageUrlList = JsonHelper.fromJsonToList(review.getImageUrl(), new TypeReference<>() {});

        List<TemplateChoiceResponse> choiceResponseList = readTemplateChoiceByReview(review);
        return ReviewReportResponse.of(review, imageUrlList, choiceResponseList, reviewReport);
    }

    @Transactional
    public ReviewReportResponse updateReviewReport(Long reviewReportId, UpdateReviewReportStatusRequest request){

        ReviewReport reviewReport = reviewReportDslRepository.findByIdWithReportCode(reviewReportId)
                .orElseThrow(() -> new NotFoundException(ErrorCode.REVIEW_REPORT_NOT_FOUND));

        ReviewReportStatus reviewReportStatus = ReviewReportStatus.of(request.reviewReportStatus());
        reviewReport.updateReviewReportStatus(reviewReportStatus);

        Review review = reviewReport.getReview();
        List<String> imageUrlList = JsonHelper.fromJsonToList(review.getImageUrl(), new TypeReference<>() {});

        List<TemplateChoiceResponse> choiceResponseList = readTemplateChoiceByReview(review);
        return ReviewReportResponse.of(review, imageUrlList, choiceResponseList, reviewReport);

    }

    @Transactional
    public void cancelReviewReport(Long reviewReportId, CancelReviewReportRequest request){
        ReviewReport reviewReport = reviewReportDslRepository.findByIdWithReportCode(reviewReportId)
                .orElseThrow(() -> new NotFoundException(ErrorCode.REVIEW_REPORT_NOT_FOUND));

      reviewReport.cancel(request.cancelReason());
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
