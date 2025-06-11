package turtleMart.review.repository;

import jakarta.annotation.Nullable;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import turtleMart.global.common.CursorPageResponse;
import turtleMart.review.entity.Review;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

public interface ReviewDslRepository {

    Optional<Review> findByIdWithChoice(Long reviewId);

    Page<Review> findByMemberIdWithPagination(Long memberId, Pageable pageable);

    List<Review> findByProductWithSearch(Long productId, String keyWord, Integer rating, Integer cursor);

    List<Review> findByIdInWithPagination(List<Long> reviewIdList);

    CursorPageResponse<Review> findAllPendingSync(LocalDateTime lastSyncedAt, LocalDateTime startSyncTime, Long lastCursor);

    void updateSyncStatus(List<Long> reviewIdList);
}
