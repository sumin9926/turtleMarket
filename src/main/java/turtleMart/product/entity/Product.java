package turtleMart.product.entity;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import turtleMart.global.common.BaseEntity;
import turtleMart.member.entity.Seller;
import turtleMart.product.dto.ProductRequest;

@Entity
@Getter
@Table
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class Product extends BaseEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne
    @JoinColumn
    private Seller seller;

    private String name;

    private Integer price;

    private String description;

    private boolean isDeleted;

    private Product(Seller seller, String name, Integer price, String description) {
        this.seller = seller;
        this.name = name;
        this.price = price;
        this.description = description;
    }

    public static Product of(Seller seller, String name, Integer price, String description) {
        return new Product(seller, name, price, description);
    }

    public void update(ProductRequest productRequest) {
        this.name = productRequest.name();
        this.description = productRequest.description();
        this.price = productRequest.price();
    }
}
