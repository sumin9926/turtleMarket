package turtleMart.review.service;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import turtleMart.member.entity.Member;
import turtleMart.member.repository.MemberRepository;
import turtleMart.review.dto.request.CreateReviewReportRequest;
import turtleMart.review.dto.response.ReviewReportResponse;
import turtleMart.review.dto.response.ReviewResponse;
import turtleMart.review.entity.ReasonCode;
import turtleMart.review.entity.Review;
import turtleMart.review.entity.ReviewReport;
import turtleMart.review.repository.ReasonCodeRepository;
import turtleMart.review.repository.ReviewReportRepository;
import turtleMart.review.repository.ReviewRepository;

@Service
@RequiredArgsConstructor
public class ReviewReportService {

    private final MemberRepository memberRepository;
    private final ReviewRepository reviewRepository;
    private final ReasonCodeRepository reasonCodeRepository;
    private final ReviewReportRepository reviewReportRepository;

    public ReviewReportResponse createReviewReport(Long memberId, Long reviewId, CreateReviewReportRequest request){

//        if(memberRepository.existsById(memberId)){throw new RuntimeException("존재하지 않는 멤버입니다");}
//        Member member = memberRepository.getReferenceById(memberId);
//
//        if(reviewRepository.existsById(reviewId)){throw new RuntimeException("존재하지 않는 리뷰입니다");}
//        Review review = reviewRepository.getReferenceById(reviewId);
//
//        if(reasonCodeRepository.existsById(request.reasonCodeId())){throw new RuntimeException("신고 코드입니다");}
//        ReasonCode reasonCode = reasonCodeRepository.getReferenceById(request.reasonCodeId());
//
//        ReviewReport reviewReport = reviewReportRepository.save(ReviewReport.of(review, member, reasonCode, request.ReasonDetail()));
//
//        ReviewResponse.of(review, )
//        return ReviewReportResponse.of( reviewReport)
        return null;


    }
}
