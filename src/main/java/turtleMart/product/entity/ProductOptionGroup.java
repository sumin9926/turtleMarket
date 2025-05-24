package turtleMart.product.entity;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import turtleMart.global.common.BaseEntity;

import java.util.ArrayList;
import java.util.List;

@Entity
@Getter
@Table
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class ProductOptionGroup extends BaseEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private String name;

    @OneToMany(mappedBy = "productOptionGroup", cascade = CascadeType.ALL, orphanRemoval = true)
    private List<ProductOptionValue> productOptionValueList = new ArrayList<>();

    private ProductOptionGroup(String name) {
        this.name = name;
    }

    public static ProductOptionGroup of(String name) {
        return new ProductOptionGroup(name);
    }

    public boolean duplicate(String name) {
        return productOptionValueList.stream().anyMatch(o -> o.getName().equals(name));
    }

    public void addValue(ProductOptionValue productOptionValue) {
        productOptionValue.addGroup(this);
        productOptionValueList.add(productOptionValue);
    }
}
