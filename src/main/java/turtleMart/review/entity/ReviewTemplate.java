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
    private String low;

    @Column(length = 30, nullable = false)
    private String medium;

    @Column(length = 30, nullable = false)
    private String high;

    private ReviewTemplate(String question, String low, String medium, String high){
        this.question = question;
        this.low = low;
        this.medium = medium;
        this.high = high;
    }

    public static ReviewTemplate of(String question, String low, String medium, String high){
        return new ReviewTemplate(question, low, medium, high);
    }
}
