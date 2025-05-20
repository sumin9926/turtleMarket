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
    private String bad;

    @Column(length = 30, nullable = false)
    private String normal;

    @Column(length = 30, nullable = false)
    private String good;

    private ReviewTemplate(String question, String bad, String normal, String good){
        this.question = question;
        this.bad = bad;
        this.normal = normal;
        this.good = good;
    }

    public static ReviewTemplate of(String question, String bad, String normal, String good){
        return new ReviewTemplate(question, bad, normal, good);
    }
}
