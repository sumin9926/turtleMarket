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

        if (request.title() != null && !request.title().isEmpty()) {updateFiledMap.put("title", request.title());}
        if (request.content() != null && !request.content().isEmpty()) {updateFiledMap.put("content", request.content());}
        if (request.rating() != null) {updateFiledMap.put("rating", request.rating());}

        UpdateRequest<ReviewDocument, Map<String, Object>> updateRequest =
                UpdateRequest.of(b -> b
                .index("review")
                .id(String.valueOf(id))
                .doc(updateFiledMap)
                .detectNoop(true)
                .docAsUpsert(true)
        );

        try {
            client.update(updateRequest, Void.class);
        } catch (IOException ex) {
            log.error("review document 업데이트 중 문제가 발생하였습니다. reviewId: {}" , id);// 이경우 비동기처리하는것 고려중,,
        }
    }


    public List<Long> searchByCondition(String keyword, Long productId, Integer rating, Pageable pageable) {

        BoolQuery.Builder builder = QueryBuilders.bool();

        builder.filter(Query.of(q -> q.term(m -> m.field("productId").value(productId))));

        if (rating != null) {
            builder.filter(Query.of(q -> q.term(m -> m.field("rating").value(rating))));
        }

        if (keyword != null && !keyword.isEmpty()) {// 키워드가 공백이거나 특수문자면 유효하지 않은 요청으로 처리
            builder.should(Query.of(q -> q.match(m -> m.field("title").fuzziness("2").query(keyword))));
            builder.should(Query.of(q -> q.match(m -> m.field("content").fuzziness("2").query(keyword))));
            builder.minimumShouldMatch("1");
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
         throw new RuntimeException("검색 중 알수 없는 예외가 발생하였습니다.");
        }
    }


}
