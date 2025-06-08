package turtleMart.review.service;

import lombok.RequiredArgsConstructor;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Component;
import turtleMart.global.kafka.enums.KafkaConst;
import turtleMart.global.utill.JsonHelper;
import turtleMart.review.entity.ReviewDocument;
import turtleMart.review.repository.ReviewElasticSearchQueryClient;

import java.io.IOException;

@Component
@RequiredArgsConstructor
public class ReviewListener {

    private final ReviewElasticSearchQueryClient client;

    @KafkaListener(topics = KafkaConst.REVIEW_SYNC_TOPIC, groupId = "document-update-sync")
    public void elasticSearchSync(String payload) throws IOException{

        ReviewDocument reviewDocument = JsonHelper.fromJson(payload, ReviewDocument.class);
        client.updateDocumentRetry(reviewDocument);
    }//dtl 구현하기

    //실패건이 많을수 있을때를 대비해서 배치처리로 몇개씩 묶어서 요청을 보낼수도 있게
    //배치 사이즈만큼 안채워진다면 동기화처리 이후 특정시점에 시도
}
