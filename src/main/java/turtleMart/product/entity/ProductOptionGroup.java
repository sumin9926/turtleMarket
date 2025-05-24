package turtleMart.product.entity;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import turtleMart.global.common.BaseEntity;
import turtleMart.global.exception.ErrorCode;
import turtleMart.global.exception.NotFoundException;
import turtleMart.product.dto.ProductOptionGroupRequest;
import turtleMart.product.dto.ProductOptionValueRequest;
import turtleMart.product.dto.ProductOptionValueUpdateRequest;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

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

    @Transient
    private Map<Long, ProductOptionValue> optionValueMapCache;

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

    public void update(ProductOptionGroupRequest productOptionGroupRequest) {
        if (!this.name.equals(productOptionGroupRequest.name())) {
            this.name = productOptionGroupRequest.name();
        }
    }

    public void updateOptionValue(ProductOptionValueUpdateRequest optionValueRequest) {
        ProductOptionValue productOptionValue = getOptionValueById(optionValueRequest.id());
        productOptionValue.update(optionValueRequest.name());
    }

    public ProductOptionValue getOptionValueById(Long id) {
        if (optionValueMapCache == null || optionValueMapCache.size() != productOptionValueList.size()) {
            optionValueMapCache = productOptionValueList.stream()
                    .collect(Collectors.toMap(ProductOptionValue::getId, v -> v));
        }
        return Optional.ofNullable(optionValueMapCache.get(id))
                .orElseThrow(() -> new NotFoundException(ErrorCode.OPTION_VALUE_NOT_FOUND));
    }
}
