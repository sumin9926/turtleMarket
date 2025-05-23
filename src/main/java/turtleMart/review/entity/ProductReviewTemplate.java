package turtleMart.review.entity;


import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import turtleMart.product.entity.Product;


@Getter
 @Entity
@Table(uniqueConstraints = @UniqueConstraint(columnNames = {"product_id", "review_template_id"}))
@NoArgsConstructor(access = AccessLevel.PROTECTED)

public class ProductReviewTemplate {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "product_id")
    private Product product;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "review_template_id")
    private ReviewTemplate reviewTemplate;


    private ProductReviewTemplate(Product product, ReviewTemplate reviewTemplate){
        this.product = product;
        this.reviewTemplate = reviewTemplate;
    }

    public static ProductReviewTemplate of(Product product, ReviewTemplate reviewTemplate){
        return new ProductReviewTemplate(product, reviewTemplate);
    }
}
