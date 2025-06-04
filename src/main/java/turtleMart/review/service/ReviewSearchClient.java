package turtleMart.review.service;

import co.elastic.clients.elasticsearch.ElasticsearchClient;
import co.elastic.clients.elasticsearch._types.query_dsl.BoolQuery;
import co.elastic.clients.elasticsearch._types.query_dsl.Query;
import co.elastic.clients.elasticsearch._types.query_dsl.QueryBuilders;
import co.elastic.clients.elasticsearch.core.SearchRequest;
import co.elastic.clients.elasticsearch.core.SearchResponse;
import co.elastic.clients.elasticsearch.core.UpdateRequest;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Component;
import turtleMart.review.dto.request.UpdateReviewRequest;
import turtleMart.review.entity.ReviewDocument;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Slf4j
@Component
@RequiredArgsConstructor
public class ReviewSearchClient {

    private final ElasticsearchClient client;

    public void updateReviewDocument(Long id, UpdateReviewRequest request) {

        Map<String, Object> updateFiledMap = new HashMap<>();

        if (request.title() != null && request.title().isEmpty()) {
            updateFiledMap.put("title", request.title());
        }
        if (request.content() != null && request.content().isEmpty()) {
            updateFiledMap.put("content", request.content());
        }
        if (request.rating() != null) {
            updateFiledMap.put("rating", request.rating());
        }

        UpdateRequest<ReviewDocument, Map<String, Object>> updateRequest = UpdateRequest.of(b -> b
                .index("members")
                .id("1")
                .doc(updateFiledMap)  // 동적으로 Map 주입 가능!
                .docAsUpsert(true)
        );

        try {
            client.update(updateRequest, Void.class);
        } catch (Exception ex) {
            log.error("review document를 업데이트 중 문제가 발생하였습니다. id: +" + id);
        }
    }


    public List<Long> searchByCondition(String keyword, Long productId, Integer rating, Pageable pageable) {

        BoolQuery.Builder builder = QueryBuilders.bool();

        builder.filter(Query.of(q -> q.term(m -> m.field("productId").value(productId))));

        if (keyword != null && !keyword.isEmpty()) {
            builder.should(Query.of(q -> q.match(m -> m.field("title").query(keyword))));
            builder.should(Query.of(q -> q.match(m -> m.field("content").query(keyword))));
            builder.minimumShouldMatch("1");
        }

        if (rating != null) {
            builder.filter(Query.of(q -> q.term(m -> m.field("rating").value(rating))));
        }

        SearchRequest searchRequest = new SearchRequest.Builder()
                .index("review")
                .from(pageable.getPageNumber() * pageable.getPageSize())
                .size(pageable.getPageSize())
                .query(q -> q.bool(builder.build())).build();

        try {
           SearchResponse<ReviewDocument> searchResponse = client.search(searchRequest, ReviewDocument.class);
           return searchResponse.hits().hits().stream().map(h -> Long.parseLong(h.id())).toList();

        } catch (IOException ex) {
         throw new RuntimeException("검색 중 문제 발생");
        }
    }


}
