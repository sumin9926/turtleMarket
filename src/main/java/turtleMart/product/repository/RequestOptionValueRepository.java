package turtleMart.product.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import turtleMart.product.entity.RequestOptionGroup;
import turtleMart.product.entity.RequestOptionValue;

public interface RequestOptionValueRepository extends JpaRepository<RequestOptionValue,Long> {
}
