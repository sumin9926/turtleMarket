package turtleMart.review.entity;


import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import turtleMart.review.dto.request.UpdateReviewTemplateRequest;

import java.awt.*;

@Getter
@Table @Entity
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class ReviewTemplate {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(length = 50, nullable = false)
    private String question;

    @Column(length = 30, nullable = false)
    private String low;

    @Column(length = 30, nullable = false)
    private String medium;

    @Column(length = 30, nullable = false)
    private String high;

    private boolean isDeleted = false;

    private ReviewTemplate(String question, String low, String medium, String high){
        this.question = question;
        this.low = low;
        this.medium = medium;
        this.high = high;
    }

    public static ReviewTemplate of(String question, String low, String medium, String high){
        return new ReviewTemplate(question, low, medium, high);
    }

    public void update(UpdateReviewTemplateRequest request){
        this.question = request.question();
        this.low = request.low();
        this.medium = request.medium();
        this.high = request.high();
    }

    public void delete(){
        this.isDeleted = true;
    }

    public String getChoice(TemplateChoiceGrade choiceGrade){

        if(TemplateChoiceGrade.LOW == choiceGrade){return this.low;}
        if(TemplateChoiceGrade.MEDIUM == choiceGrade){return this.medium;}
        if(TemplateChoiceGrade.HIGH == choiceGrade){return this.high;}
        return null;
    }
}
