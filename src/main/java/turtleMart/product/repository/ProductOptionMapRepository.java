package turtleMart.product.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import turtleMart.product.entity.ProductOptionMap;

public interface ProductOptionMapRepository extends JpaRepository<ProductOptionMap, Long> {

}
