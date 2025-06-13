package turtleMart.review.service;

import com.fasterxml.jackson.core.type.TypeReference;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import turtleMart.global.common.CursorPageResponse;
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
import turtleMart.review.repository.ReasonCodeRepository;
import turtleMart.review.repository.ReviewDslRepositoryImpl;
import turtleMart.review.repository.ReviewReportDslRepositoryImpl;
import turtleMart.review.repository.ReviewReportRepository;
import turtleMart.security.CheckRole;

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
    public ReviewReportResponse createReviewReport(Long memberId, Long reviewId, CreateReviewReportRequest request) {

        if(!memberRepository.existsById(memberId)){throw new NotFoundException(ErrorCode.MEMBER_NOT_FOUND);}
        if(!reasonCodeRepository.existsById(request.reasonCodeId())){throw new NotFoundException(ErrorCode.REVIEW_REPORT_NOT_FOUND);}

        Member member = memberRepository.getReferenceById(memberId);
        ReasonCode reasonCode = reasonCodeRepository.getReferenceById(request.reasonCodeId());

        Review review = reviewDslRepository.findByIdWithChoice(reviewId)
                                .orElseThrow(() -> new NotFoundException(ErrorCode.REVIEW_NOT_FOUND));

        if(reviewReportRepository.existsByMemberIdAndReviewId(member.getId(), review.getId())){
            throw new BadRequestException(ErrorCode.DUPLICATE_REVIEW_REPORT);
        }

        ReviewReport reviewReport = reviewReportRepository.save(ReviewReport.of(review, member, reasonCode, request.ReasonDetail()));

        List<TemplateChoiceResponse> choiceResponseList = TemplateChoice.changeResponseByReview(review);
        List<String> imageUrlList = JsonHelper.fromJsonToList(review.getImageUrl(), new TypeReference<>(){});
        return ReviewReportResponse.of(review, imageUrlList, choiceResponseList, reviewReport);
    }


    @CheckRole("ADMIN")
    public ReviewReportResponse readById(Long reviewReportId) {
        ReviewReport reviewReport = reviewReportDslRepository.findByIdWithReportCode(reviewReportId)
                .orElseThrow(() -> new NotFoundException(ErrorCode.REVIEW_REPORT_NOT_FOUND));

        Review review = reviewReport.getReview();
        List<String> imageUrlList = JsonHelper.fromJsonToList(review.getImageUrl(), new TypeReference<>(){});
        List<TemplateChoiceResponse> choiceResponseList = TemplateChoice.changeResponseByReview(review);
        return ReviewReportResponse.of(review, imageUrlList, choiceResponseList, reviewReport);
    }

    @CheckRole("ADMIN")
    public CursorPageResponse<ReviewReportResponse> readByCondition(String reviewReportStatus, String reasonCode, Integer size, Long cursor) {
        List<ReviewReport> reviewReportList = reviewReportDslRepository.findByReviewReportCondition(reviewReportStatus, reasonCode, cursor);

        List<ReviewReportResponse> reportResponseList = reviewReportList.stream().map(r -> {
            Review review = r.getReview();
            List<String> imageUrlList = JsonHelper.fromJsonToList(review.getImageUrl(), new TypeReference<>(){});
            List<TemplateChoiceResponse> choiceResponseList = TemplateChoice.changeResponseByReview(review);
            return ReviewReportResponse.of(review, imageUrlList, choiceResponseList, r);
        }).toList();

        if(reportResponseList.isEmpty()){return CursorPageResponse.of(reportResponseList, 0L, true);}

        boolean hasNext = reportResponseList.size() <= size;
        if(hasNext){reportResponseList.subList(0, reportResponseList.size() - 1);}
        Long lastCursor = reportResponseList.get(reportResponseList.size() - 1).id();

        return CursorPageResponse.of(reportResponseList, lastCursor, hasNext);
    }

    @CheckRole("ADMIN")
    @Transactional
    public ReviewReportResponse updateReviewReport(Long memberId, Long reviewReportId, UpdateReviewReportStatusRequest request){
        if(!memberRepository.existsById(memberId)){throw new NotFoundException(ErrorCode.MEMBER_NOT_FOUND);}

        ReviewReport reviewReport = reviewReportDslRepository.findByIdWithReportCode(reviewReportId)
                .orElseThrow(() -> new NotFoundException(ErrorCode.REVIEW_REPORT_NOT_FOUND));

        ReviewReportStatus reviewReportStatus = ReviewReportStatus.of(request.reviewReportStatus());
        reviewReport.updateReviewReportStatus(reviewReportStatus);

        Review review = reviewReport.getReview();
        List<String> imageUrlList = JsonHelper.fromJsonToList(review.getImageUrl(), new TypeReference<>(){});
        List<TemplateChoiceResponse> choiceResponseList = TemplateChoice.changeResponseByReview(review);
        return ReviewReportResponse.of(review, imageUrlList, choiceResponseList, reviewReport);

    }

    @Transactional
    public void cancelReviewReport(Long memberId, Long reviewReportId, CancelReviewReportRequest request) {
        if(!memberRepository.existsById(memberId)){throw new NotFoundException(ErrorCode.MEMBER_NOT_FOUND);}

        ReviewReport reviewReport = reviewReportDslRepository.findByIdWithReportCode(reviewReportId)
                .orElseThrow(() -> new NotFoundException(ErrorCode.REVIEW_REPORT_NOT_FOUND));

        Long writerId = reviewReport.getMember().getId();
        if(writerId.equals(memberId)){throw new BadRequestException(ErrorCode.FORBIDDEN);}

        reviewReport.cancel(request.cancelReason());
    }

}
