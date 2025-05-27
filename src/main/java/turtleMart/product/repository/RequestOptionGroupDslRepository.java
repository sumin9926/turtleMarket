package turtleMart.product.repository;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import turtleMart.product.entity.RequestOptionGroup;
import turtleMart.product.entity.RequestOptionValueStatus;

public interface RequestOptionGroupDslRepository {
    Page<RequestOptionGroup> findAllBySellerIdWithValue(Long id, Pageable pageable);

    Page<RequestOptionGroup> findAllByStatusWithSeller(Pageable pageable, RequestOptionValueStatus requestOptionValueStatus);

    RequestOptionGroup findByIdWithValue(Long aLong);
}
