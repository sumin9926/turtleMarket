package turtleMart.product.entity;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Entity
@Getter
@Table
@RequiredArgsConstructor(access = AccessLevel.PROTECTED)
public class ProductOptionValue {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private String name;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn
    private ProductOptionGroup productOptionGroup;

    private ProductOptionValue(String name) {
        this.name = name;
    }

    public static ProductOptionValue of(String name) {
        return new ProductOptionValue(name);
    }

    public void addGroup(ProductOptionGroup productOptionGroup) {
        this.productOptionGroup = productOptionGroup;
    }

    public void update(String name) {
        if (!this.name.equals(name)) {
            this.name = name;
        }
    }
}
