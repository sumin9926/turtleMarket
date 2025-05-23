package turtleMart.review.repository;

import turtleMart.review.entity.Review;

import java.util.Optional;

public interface ReviewDslRepository {

    Optional<Review> findByIdWithChoice(Long reviewId);
}
