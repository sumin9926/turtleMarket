package turtleMart.review.entity;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@Table @Entity
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class TemplateChoice {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn
    private Review review;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn
    private ProductReviewTemplate productReviewTemplate;

    @Enumerated(EnumType.STRING)
    private TemplateChoiceGrade choseAnswer; // 필드명 변경

    private TemplateChoice(ProductReviewTemplate productReviewTemplate, TemplateChoiceGrade choseAnswer){
        this.productReviewTemplate = productReviewTemplate;
        this.choseAnswer = choseAnswer;
    }

    public static TemplateChoice of(ProductReviewTemplate productReviewTemplate, TemplateChoiceGrade choseAnswer){
        return new TemplateChoice(productReviewTemplate, choseAnswer);
    }

    public void setReview(Review review){
        this.review = review;
        review.templateChoiceList.add(this);
    }

    public void update(TemplateChoiceGrade choseAnswer){this.choseAnswer = choseAnswer;}
}
