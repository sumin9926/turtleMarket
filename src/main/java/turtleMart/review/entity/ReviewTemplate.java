package turtleMart.review.entity;


import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

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
    private String satisfaction_low;

    @Column(length = 30, nullable = false)
    private String satisfaction_medium;

    @Column(length = 30, nullable = false)
    private String satisfaction_high;

    private boolean isDeleted = false;

    private ReviewTemplate(String question, String satisfaction_low, String satisfaction_medium, String satisfaction_high){
        this.question = question;
        this.satisfaction_low = satisfaction_low;
        this.satisfaction_medium = satisfaction_medium;
        this.satisfaction_high = satisfaction_high;
    }

    public static ReviewTemplate of(String question, String low, String medium, String high){
        return new ReviewTemplate(question, low, medium, high);
    }

    public void update(String question, String satisfaction_low, String satisfaction_medium, String satisfaction_high){
        this.question = question;
        this.satisfaction_low = satisfaction_low;
        this.satisfaction_medium = satisfaction_medium;
        this.satisfaction_high = satisfaction_high;
    }

    public void delete(){this.isDeleted = true;}

    public String getChoice(TemplateChoiceGrade choiceGrade){
        switch (choiceGrade){
            case LOW -> {return this.satisfaction_low;}
            case MEDIUM -> {return this.satisfaction_medium;}
            case HIGH -> {return this.satisfaction_high;}
            default -> throw new RuntimeException("존재하지 않는 TemplateChoiceGrade입니다");
        }
    }
}
