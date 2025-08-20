package turtleMart.product.repository;

import jakarta.persistence.LockModeType;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.data.jpa.repository.Query;
import turtleMart.product.entity.ProductOptionCombination;

import java.util.List;
import java.util.Optional;

public interface ProductOptionCombinationRepository extends JpaRepository<ProductOptionCombination, Long> {
    List<ProductOptionCombination> findAllByProductId(Long productId);

    boolean existsByProductIdAndUniqueKey(Long productId, String uniqueKey);

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    @Query("SELECT p FROM ProductOptionCombination p WHERE p.id = :productOptionCombinationId")
    Optional<ProductOptionCombination> findByIdWithPessimisticLock(Long productOptionCombinationId);

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    @EntityGraph(attributePaths = "product")
    List<ProductOptionCombination> findAllByIdIn(List<Long> idList);
}
