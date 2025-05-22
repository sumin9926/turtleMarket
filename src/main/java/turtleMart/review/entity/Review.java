package turtleMart.review.entity;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;
import turtleMart.global.common.BaseEntity;
import turtleMart.member.entity.Member;
import turtleMart.order.entity.OrderItem;
import turtleMart.product.entity.Product;
import turtleMart.review.dto.request.UpdateReviewRequest;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@Getter
@Entity @Table
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class Review extends BaseEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn
    private Member member;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn
    private Product product;

    @OneToOne(fetch = FetchType.LAZY)
    @JoinColumn
    private OrderItem orderItem;

    @OneToMany(mappedBy = "review", cascade =  CascadeType.ALL)
    List<TemplateChoice> templateChoiceList = new ArrayList<>();

    @Column(length = 20, nullable = false)
    private String title;

    @Column(length = 255, nullable = false)
    private String content;

    @Column(columnDefinition = "TINYINT", nullable = false)
    private Integer rating;

    @Column(columnDefinition = "TEXT", nullable = false)
    private String imageUrl;

    private Review(Member member, Product product, OrderItem orderItem, String title, String content, Integer rating, String imageUrl, List<TemplateChoice> templateChoiceList) {
        this.member = member;
        this.product = product;
        this.orderItem = orderItem;
        this.title = title;
        this.content = content;
        this.rating = rating;
        this.imageUrl = imageUrl;
        this.templateChoiceList = templateChoiceList;
    }

    public static Review of(Member member, Product product, OrderItem orderItem, String title, String content, Integer rating, String imageUrl, List<TemplateChoice> templateChoiceList) {
        return new Review(member, product, orderItem, title, content, rating, imageUrl, templateChoiceList);
    }

    public void update(UpdateReviewRequest request, String imageUrl) {
        this.title = request.title();
        this.content = request.content();
        this.rating = request.rating();
        this.imageUrl = imageUrl;
    }
}
