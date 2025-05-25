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

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "seller_id")
    private Seller seller;

    private String name;

    private Integer price;

    private String description;

    @Column(name = "deleted")
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
        if (!this.name.equals(productRequest.name())) {
            this.name = productRequest.name();
        }
        if (!this.description.equals(productRequest.description())) {
            this.description = productRequest.description();
        }
        if (!this.price.equals(productRequest.price())) {
            this.price = productRequest.price();
        }
    }

    public void delete(boolean choice) {
        this.isDeleted = choice;
    }
}
