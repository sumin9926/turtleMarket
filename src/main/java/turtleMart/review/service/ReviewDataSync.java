package turtleMart.review.service;

import co.elastic.clients.elasticsearch.ElasticsearchClient;
import co.elastic.clients.elasticsearch.core.BulkRequest;
import co.elastic.clients.elasticsearch.core.BulkResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.retry.support.RetryTemplate;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import turtleMart.global.common.CursorPageResponse;
import turtleMart.global.common.ElasticSearchConst;
import turtleMart.review.entity.Review;
import turtleMart.review.entity.ReviewDocument;
import turtleMart.review.repository.ReviewDslRepositoryImpl;
import turtleMart.review.repository.ReviewElasticSearchQueryClient;
import java.io.IOException;
import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Slf4j
@Component
@RequiredArgsConstructor
public class ReviewDataSync {

    private final ReviewDslRepositoryImpl reviewDslRepository;
    private final ReviewService reviewService;
    private final ElasticsearchClient client;
    private final ReviewElasticSearchQueryClient elasticSearchQueryClient;
    private final RedisTemplate<String, String> stringRedisTemplate;

    private final RetryTemplate retryTemplate = new RetryTemplate();

    private LocalDateTime lastSyncTime;
    private final String LAST_SYNC_CHECK_POINT = "lastSyncTime";

    @Scheduled(cron = "0 0 2 * * *")
    public void elsaDataSync() {
        Long lastCursor = 0L;

        String redisValue = stringRedisTemplate.opsForValue().get(LAST_SYNC_CHECK_POINT);
        lastSyncTime = redisValue == null ? LocalDateTime.now() : LocalDateTime.parse(redisValue);

        LocalDateTime startSyncTime = LocalDateTime.now();

        BulkRequest.Builder requestBuilder = new BulkRequest.Builder();
        Map<Long, ReviewDocument> reviewDocumentMap = new HashMap<>();

        CursorPageResponse<Review> reviewPage;
        do{
            reviewPage = reviewDslRepository.findAllPendingSync(lastSyncTime, startSyncTime, lastCursor);
            createBulkRequest(requestBuilder, reviewPage.content(), reviewDocumentMap);
            lastCursor = reviewPage.lastCursor();
        }while (!reviewPage.isLastPage());


        BulkResponse response;
        try {
            response = client.bulk(requestBuilder.build());// 모든데이터에 대한 벌크 쿼리를 한번에 보낸다
        } catch (IOException ex) {
            log.error("동기화 중 알 수 없는 문제 발생 - reason:{}", ex.getMessage());
            return;
        }

        List<ReviewDocument> errorLogList = response.items().stream()//에러가 발생한 도큐먼트를 뽑아온다
                .filter(r -> r.error() != null)
                .map(i -> reviewDocumentMap.get(Long.parseLong(i.id())))
                .toList();

        if(response.errors()){retryFailedSync(errorLogList);}

        reviewService.successDataStatusChange(response.items());
        stringRedisTemplate.opsForValue().set(LAST_SYNC_CHECK_POINT, startSyncTime.toString());
    }

    private void createBulkRequest(BulkRequest.Builder bulkQueryCreator, List<Review> reviewList, Map<Long, ReviewDocument> reviewDocumentMap) {
        for (Review review : reviewList) {
            ReviewDocument reviewDocument = ReviewDocument.from(review);

            bulkQueryCreator.operations(op -> op
                    .update(upd -> upd
                            .index(ElasticSearchConst.REVIEW_INDEX)
                            .id(review.getId().toString())
                            .action(a -> a
                                    .doc(reviewDocument)
                                    .detectNoop(true)
                                    .docAsUpsert(true)
                            )
                    )
            );
            reviewDocumentMap.put(reviewDocument.getId(), reviewDocument);
        }
    }


    public void retryFailedSync(List<ReviewDocument> failedList) {
        for (ReviewDocument reviewDocument : failedList) {
            retryTemplate.execute(context -> {
                elasticSearchQueryClient.updateDocumentRetry(reviewDocument);
                return null;
            }, context -> {
                Throwable lastError = context.getLastThrowable();
                log.warn("모든 재시도 실패. 엘라스틱 문서 동기화 실패. docId={}, 이유={}",
                        reviewDocument.getId(), lastError.toString());
                return null;
            });
        }
        }
    }
