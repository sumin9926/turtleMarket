package turtleMart.review.entity;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;
import turtleMart.member.entity.Member;
import turtleMart.order.entity.OrderItem;
import turtleMart.product.Product;

import java.time.LocalDateTime;
@Getter
@Entity @Table
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class Review {

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

    @Column(length = 20, nullable = false)
    private String title;

    @Column(length = 255, nullable = false)
    private String content;

    @Column(columnDefinition = "TINYINT", nullable = false)
    private Integer rating;

    @Column(columnDefinition = "TEXT", nullable = false)
    private String imageUrl;

    @CreatedDate
    @Column(updatable = false)
    private LocalDateTime createdAt;

    @LastModifiedDate
    private LocalDateTime updatedAt;

    private Review(Member member, Product product, OrderItem orderItem, String title, String content, Integer rating, String imageUrl){
        this.member = member;
        this.product = product;
        this.orderItem = orderItem;
        this.title = title;
        this.content = content;
        this.rating = rating;
        this.imageUrl = imageUrl;
    }

    public static Review of(Member member, Product product, OrderItem orderItem, String title, String content, Integer rating, String imageUrl){
        return new Review(member, product, orderItem, title, content, rating, imageUrl);
    }
}
