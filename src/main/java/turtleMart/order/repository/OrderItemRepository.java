package turtleMart.order.repository;

import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import turtleMart.order.entity.OrderItem;

import java.util.List;

public interface OrderItemRepository extends JpaRepository<OrderItem, Long> {

    boolean existsByProductOptionCombinationId(Long productOptionCombinationId);

    boolean existsByIdAndProductOptionCombination_Product_SellerId(Long orderItemId, Long SellerId);

    @Query("SELECT o FROM OrderItem o LEFT JOIN FETCH o.productOptionCombination WHERE o.order.id = :orderId")
    List<OrderItem> findAllByOrderId(@Param("orderId")Long orderId);

    @EntityGraph(attributePaths = "productOptionCombination")
    List<OrderItem> findAllByIdIn(List<Long> orderItemIdList);
}
