package turtleMart.review.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import turtleMart.review.entity.Review;

public interface ReviewRepository extends JpaRepository<Review, Long> {
    Boolean existsByOrderItemIdAndIsDeletedFalse(Long orderItemId);
}
