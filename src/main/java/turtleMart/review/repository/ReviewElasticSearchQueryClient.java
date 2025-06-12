package turtleMart.review.repository;

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
import turtleMart.global.common.ElasticSearchConst;
import turtleMart.global.exception.CustomRuntimeException;
import turtleMart.global.exception.ErrorCode;
import turtleMart.global.exception.ExternalServiceException;
import turtleMart.review.dto.request.UpdateReviewRequest;
import turtleMart.review.entity.Review;
import turtleMart.review.entity.ReviewDocument;

import java.io.IOException;
import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class ReviewElasticSearchQueryClient {

    private final ElasticsearchClient client;
    private final ReviewElasticSearchRepository reviewElasticSearchRepository;

    public void createReviewDocument(Review review) {
        ReviewDocument reviewDocument = ReviewDocument.from(review);
        reviewElasticSearchRepository.save(reviewDocument);
    }

    public List<Long> searchByCondition(String keyword, Long productId, Integer rating, Pageable pageable) {

        BoolQuery.Builder builder = QueryBuilders.bool();

        builder.filter(Query.of(q -> q.term(m -> m.field("productId").value(productId))));

        if (rating != null) {
            builder.filter(Query.of(q -> q.term(m -> m.field("rating").value(rating))));
        }

        if (keyword != null && !keyword.isEmpty()) {
            builder.should(Query.of(q -> q.match(m -> m.field("title").fuzziness("2").query(keyword))));
            builder.should(Query.of(q -> q.match(m -> m.field("content").fuzziness("2").query(keyword))));
            builder.minimumShouldMatch("1");
        }

        SearchRequest searchRequest = new SearchRequest.Builder()
                .index(ElasticSearchConst.REVIEW_INDEX)
                .from(pageable.getPageNumber() * pageable.getPageSize())
                .size(pageable.getPageSize())
                .query(q -> q.bool(builder.build())).build();

        try {
            SearchResponse<ReviewDocument> searchResponse = client.search(searchRequest, ReviewDocument.class);
            return searchResponse.hits().hits().stream().map(h -> Long.parseLong(h.id())).toList();

        } catch (IOException ex) {
            throw new ExternalServiceException(ErrorCode.SEARCH_ERROR_RETRY_LATER);
        }
    }

    public void deleteReviewDocument(Long reviewId) {
        reviewElasticSearchRepository.deleteById(reviewId);
    }

    public void updateDocumentRetry(ReviewDocument reviewDocument) {
        UpdateRequest<ReviewDocument, ReviewDocument> request =  UpdateRequest.of(b -> b
                .index(ElasticSearchConst.REVIEW_INDEX)
                .id(String.valueOf(reviewDocument.getId()))
                .doc(reviewDocument)
                .detectNoop(true)
                .docAsUpsert(true)
        );

        try{
            client.update(request, Void.class);
        }catch (IOException ex){
            log.info("동기화 중 알수 없는 문제 발생");
        }

    }
}
