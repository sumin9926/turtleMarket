package turtleMart.product.repository;

import org.springframework.data.domain.Page;
import org.springframework.data.jpa.repository.JpaRepository;
import turtleMart.product.entity.ProductOptionCombination;

import java.awt.print.Pageable;
import java.util.List;

public interface ProductOptionCombinationRepository extends JpaRepository<ProductOptionCombination, Long> {
    List<ProductOptionCombination> findAllByProductId(Long productId);
}
