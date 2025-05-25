package turtleMart.product.entity;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

@Entity
@Getter
@Table
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class ProductOptionCombination {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "product_id")
    private Product product;

    private Integer price;

    private Integer inventory;

    @OneToMany(mappedBy = "productOptionCombination", cascade = CascadeType.ALL, orphanRemoval = true)
    private List<ProductOptionMap> productOptionMapList = new ArrayList<>();

    private ProductOptionCombination(Product product, Integer price, Integer inventory) {
        this.product = product;
        this.price = price;
        this.inventory = inventory;
    }

    public static ProductOptionCombination of(Product product, Integer price, Integer inventory) {
        return new ProductOptionCombination(product, price, inventory);
    }

    public void addOptionMap(ProductOptionMap productOptionMap) {
        this.productOptionMapList.add(productOptionMap);
    }
}
