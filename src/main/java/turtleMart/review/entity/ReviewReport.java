package turtleMart.review.entity;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import turtleMart.global.common.BaseEntity;
import turtleMart.member.entity.Member;

@Getter
@Entity @Table
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class ReviewReport extends BaseEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn
    private Review review;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn
    private Member member;

    @OneToOne(fetch = FetchType.LAZY)
    @JoinColumn
    private ReasonCode reasonCode;

    private boolean isProcessed = false;

    @Column(length = 255, nullable = false)
    private String ReasonDetail;

    @Column(length = 50, nullable = true)
    private String cancelReason;

    private ReviewReport(Review review, Member member, ReasonCode reasonCode, String reasonDetail){
        this.review = review;
        this.member = member;
        this.reasonCode = reasonCode;
        this.ReasonDetail = reasonDetail;
    }

    public static ReviewReport of(Review review, Member member, ReasonCode reasonCode, String reasonDetail){
        return new ReviewReport(review, member, reasonCode, reasonDetail);
    }

    public void update(){
        this.isProcessed = true;
    }
}
