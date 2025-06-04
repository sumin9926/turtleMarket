package turtleMart.order.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import turtleMart.order.entity.OrderItem;

public interface OrderItemRepository extends JpaRepository<OrderItem, Long> {

    boolean existsByProductOptionCombinationId(Long productOptionCombinationId);

    boolean existsByIdAndProductOptionCombination_Product_SellerId(Long orderItemId, Long SellerId);
}
