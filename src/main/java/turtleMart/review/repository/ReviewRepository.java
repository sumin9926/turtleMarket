package turtleMart.review.repository;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import turtleMart.review.entity.Review;

public interface ReviewRepository extends JpaRepository<Review, Long> {

    Boolean existsByOrderItemId(Long orderItemId);


}
