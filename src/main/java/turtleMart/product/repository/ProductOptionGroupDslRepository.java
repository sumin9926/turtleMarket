package turtleMart.product.repository;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import turtleMart.product.entity.ProductOptionGroup;

import java.util.Optional;

public interface ProductOptionGroupDslRepository {
    Page<ProductOptionGroup> findAllWithValue(Pageable pageable);
}
