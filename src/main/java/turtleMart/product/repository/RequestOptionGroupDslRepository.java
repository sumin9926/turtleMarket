package turtleMart.product.repository;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import turtleMart.product.entity.RequestOptionGroup;

public interface RequestOptionGroupDslRepository {
    Page<RequestOptionGroup> findAllBySellerIdWithValue(Long id, Pageable pageable);

    Page<RequestOptionGroup> findAllByNotYetWithSeller(Pageable pageable);

    Page<RequestOptionGroup> findAllByAllReadyWithSeller(Pageable pageable);

    RequestOptionGroup findByIdWithValue(Long aLong);
}
