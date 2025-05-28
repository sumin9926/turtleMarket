package turtleMart.review.repository;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import turtleMart.review.dto.response.ReviewResponse;
import turtleMart.review.entity.Review;

import java.util.List;
import java.util.Optional;

public interface ReviewDslRepository {

    Optional<Review> findByIdWithChoice(Long reviewId);

    List<Review> findByMemberIdWithPagination(Long memberId, Pageable pageable);
}
