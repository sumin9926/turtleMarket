package turtleMart.product.entity;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Entity
@Getter
@Table
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class ProductOptionMap {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "product_option_value_id")
    private ProductOptionValue productOptionValue;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "product_option_combination_id")
    private ProductOptionCombination productOptionCombination;

    private ProductOptionMap(ProductOptionValue productOptionValue, ProductOptionCombination productOptionCombination) {
        this.productOptionValue = productOptionValue;
        this.productOptionCombination = productOptionCombination;
    }

    public static ProductOptionMap of(ProductOptionCombination productOptionCombination, ProductOptionValue productOptionValue) {
        return new ProductOptionMap(productOptionValue, productOptionCombination);
    }
}
