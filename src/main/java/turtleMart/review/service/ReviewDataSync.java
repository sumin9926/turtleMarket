package turtleMart.review.service;

import co.elastic.clients.elasticsearch.ElasticsearchClient;
import co.elastic.clients.elasticsearch.core.BulkRequest;
import co.elastic.clients.elasticsearch.core.BulkResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import turtleMart.global.common.CursorPageResponse;
import turtleMart.global.kafka.enums.KafkaConst;
import turtleMart.global.utill.JsonHelper;
import turtleMart.review.entity.Review;
import turtleMart.review.entity.ReviewDocument;
import turtleMart.review.repository.ReviewDslRepositoryImpl;
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
    private final KafkaTemplate<String, String> stringKafkaTemplate;

    private LocalDateTime lastSyncTime;
    private final String REVIEW_INDEX = "review";
    // 맨 처음 시간을 뭘로 설정해야하는거지? - 서버를 다시 띄울때에도 유효해야하는거잖아 그러면 어딘가에다가 저장해야하나?


    @Scheduled(cron = "0 42 3 * * *")
    public void elsaDataSync() {
        log.info("동기화 시작");
        Long lastCursor = 0L;
        LocalDateTime startSyncTime = LocalDateTime.now();
        BulkRequest.Builder requestBuilder = new BulkRequest.Builder();
        Map<Long, ReviewDocument> reviewDocumentMap = new HashMap<>();

        CursorPageResponse<Review> reviewPage = reviewDslRepository.findAllPendingSync(lastSyncTime, startSyncTime, lastCursor);
        createBulkRequest(requestBuilder, reviewPage.content(), reviewDocumentMap);

        while (!reviewPage.isLastPage()) {
            reviewPage = reviewDslRepository.findAllPendingSync(lastSyncTime, startSyncTime, lastCursor);
            createBulkRequest(requestBuilder, reviewPage.content(), reviewDocumentMap);
            lastCursor = reviewPage.lastCursor();
        }

        BulkResponse response;

        try {
            response = client.bulk(requestBuilder.build());// 모든데이터에 대한 벌크 쿼리를 한번에 보낸다
        } catch (IOException ex) {
            log.error("from db to elsa 동기화 중 알수 없는 문제 발생 - reason:{}", ex.getMessage());
            return;
        }

        reviewService.successDataStatusChange(response.items());

        if (response.errors()) {sendFailMessage(response, reviewDocumentMap);}

        lastSyncTime = startSyncTime;
    }

    private void createBulkRequest(BulkRequest.Builder bulkQueryCreator, List<Review> reviewList, Map<Long, ReviewDocument> reviewDocumentMap) {
        for (Review review : reviewList) {
            ReviewDocument reviewDocument = ReviewDocument.from(review);

            bulkQueryCreator.operations(op -> op
                    .update(upd -> upd
                            .index("dont")
                            .id(review.getId().toString())
                            .action(a -> a
                                    .doc(reviewDocument)
                                    .detectNoop(true)
                                    .docAsUpsert(false)
                            )
                    )
            );
            reviewDocumentMap.put(reviewDocument.getId(), reviewDocument);
        }
    }

    private void sendFailMessage(BulkResponse response, Map<Long, ReviewDocument> reviewDocumentMap) {
        List<ReviewDocument> errorLogList = response.items().stream()//에러가 발생한 도큐먼트를 뽑아온다
                .filter(r -> r.error() != null)
                .map(i -> reviewDocumentMap.get(Long.parseLong(i.id())))
                .toList();

        errorLogList.forEach(r -> {
            String payload = JsonHelper.toJson(r);
            stringKafkaTemplate.send(KafkaConst.REVIEW_SYNC_TOPIC, payload);
        });
    }//동기화 실패 후 처리
}
