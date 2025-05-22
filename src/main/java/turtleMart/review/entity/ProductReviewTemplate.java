package turtleMart.review.entity;


import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import turtleMart.product.entity.Product;


@Getter
@Table @Entity
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class ProductReviewTemplate {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn
    private Product product;

    @OneToOne(fetch = FetchType.LAZY)
    @JoinColumn
    private ReviewTemplate reviewTemplate;

    private boolean isDeleted = false;

    private ProductReviewTemplate(Product product, ReviewTemplate reviewTemplate){
        this.product = product;
        this.reviewTemplate = reviewTemplate;
    }

    public static ProductReviewTemplate of(Product product, ReviewTemplate reviewTemplate){
        return new ProductReviewTemplate(product, reviewTemplate);
    }

    public void delete(){
        this.isDeleted = true;
    }
}
