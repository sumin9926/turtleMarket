package turtleMart.product.repository;

import turtleMart.product.entity.Product;

import java.util.List;

public interface ProductDslRepository {
    Product findByIdWithSeller(Long productId);

}
