package turtleMart.review.entity;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import turtleMart.global.common.BaseEntity;
import turtleMart.member.entity.Member;

@Getter
@Entity
@Table(
        uniqueConstraints = @UniqueConstraint(columnNames = {"member_id", "review_id"}),
        indexes = @Index(name = "idx_review_report_status_reason_code", columnList = "review_report_status, reason_code")
)

@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class ReviewReport extends BaseEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "review_id")
    private Review review;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "member_id")
    private Member member;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "reason_code")
    private ReasonCode reasonCode;

    @Enumerated(EnumType.STRING)
    @Column(name = "review_report_status")
    private ReviewReportStatus reviewReportStatus = ReviewReportStatus.SUBMITTED;

    @Column(length = 255, nullable = false)
    private String reasonDetail;

    @Column(length = 50, nullable = true)
    private String cancelReason;

    private ReviewReport(Review review, Member member, ReasonCode reasonCode, String reasonDetail){
        this.review = review;
        this.member = member;
        this.reasonCode = reasonCode;
        this.reasonDetail = reasonDetail;
    }

    public static ReviewReport of(Review review, Member member, ReasonCode reasonCode, String reasonDetail){
        return new ReviewReport(review, member, reasonCode, reasonDetail);
    }

    public void updateReviewReportStatus(ReviewReportStatus reviewReportStatus){
       this.reviewReportStatus = reviewReportStatus;
    }

    public void cancel(String cancelReason){
        this.cancelReason = cancelReason;
        this.reviewReportStatus = ReviewReportStatus.CANCELLED;
    }
}
