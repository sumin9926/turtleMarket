package turtleMart.product.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import turtleMart.product.entity.ProductOptionGroup;

public interface ProductOptionGroupRepository extends JpaRepository<ProductOptionGroup,Long> {
}
