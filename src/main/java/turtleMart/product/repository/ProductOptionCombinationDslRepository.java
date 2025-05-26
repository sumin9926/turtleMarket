package turtleMart.product.repository;

import turtleMart.product.entity.ProductOptionCombination;

import java.util.List;

public interface ProductOptionCombinationDslRepository {
    List<ProductOptionCombination> findAllByProductIdWithMapAndValue(Long productId);
}
