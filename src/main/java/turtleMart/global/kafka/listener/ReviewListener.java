package turtleMart.global.kafka.listener;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.kafka.annotation.RetryableTopic;
import org.springframework.kafka.retrytopic.DltStrategy;
import org.springframework.retry.annotation.Backoff;
import org.springframework.stereotype.Component;
import turtleMart.global.kafka.enums.KafkaConst;
import turtleMart.global.utill.JsonHelper;
import turtleMart.review.entity.ReviewDocument;
import turtleMart.review.repository.ReviewElasticSearchQueryClient;
import java.io.IOException;

@Slf4j
@Component
@RequiredArgsConstructor
public class ReviewListener {

    private final ReviewElasticSearchQueryClient client;

//    @RetryableTopic(
//            attempts = "3", backoff = @Backoff(delay = 5000, multiplier = 2.0),
//            dltStrategy = DltStrategy.FAIL_ON_ERROR,
//            dltTopicSuffix = ".dlt",
//            kafkaTemplate = "stringKafkaTemplate"
//    )
    @KafkaListener(topics = KafkaConst.REVIEW_SYNC_TOPIC, groupId = "document-update-sync")
    public void elasticSearchSync(String payload) throws IOException {
        ReviewDocument reviewDocument = JsonHelper.fromJson(payload, ReviewDocument.class);
//        client.updateDocumentRetry(reviewDocument);
    }
}
