package turtleMart.product.service;

import lombok.RequiredArgsConstructor;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.connection.Message;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import org.springframework.web.context.request.async.DeferredResult;
import turtleMart.global.common.OperationType;
import turtleMart.global.component.DeferredResultStore;
import turtleMart.global.utill.JsonHelper;
import turtleMart.product.dto.ProductOptionCombinationRedisDto;

import java.nio.charset.StandardCharsets;

@Component
@RequiredArgsConstructor
public class PriceChangePubSubListener {

    private final DeferredResultStore deferredResultStore;
    private final RedisTemplate<String, Object> redisTemplate;

    @Value("${server.id}")
    private String serverId;


    public void onMessage(Message message, byte[] pattern) {
        String payload = new String(message.getBody(), StandardCharsets.UTF_8);
        ProductOptionCombinationRedisDto productOptionCombinationRedisDto = JsonHelper.fromJson(payload, ProductOptionCombinationRedisDto.class);
        Long productCombinationId = productOptionCombinationRedisDto.productCombinationId();
        String operationId = productOptionCombinationRedisDto.operationId();
        Boolean success = productOptionCombinationRedisDto.success();
        OperationType type = productOptionCombinationRedisDto.operationType();
        switch (type) {
            case PRICE_CHANGE -> {
                responsePriceChange(operationId, success, productCombinationId);
            }
            case INVENTORY_UPDATE , COMBINATION_DELETE, INVENTORY_OVERRIDE,COMBINATION_STATUS -> {
                responseCombinationWalk(operationId,success);
            }
            default -> {

            }
        }
    }

    private void responseCombinationWalk(String operationId, Boolean success) {
        if (!operationId.startsWith(serverId + ":")) {
            return;
        }
        DeferredResult<ResponseEntity<?>> result = deferredResultStore.remove(operationId);
        if (result == null) {
            return;
        }
        if (Boolean.TRUE.equals(success)) {
            result.setResult(ResponseEntity.ok().build());
        } else {
            result.setResult(ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build());
        }

        String redisKey = "status:" + operationId;
        redisTemplate.delete(redisKey);
    }

    private void responsePriceChange(String operationId, Boolean success, Long productCombinationId) {
        if (!operationId.startsWith(serverId + ":")) {
            return;
        }
        DeferredResult<ResponseEntity<?>> result = deferredResultStore.remove(operationId);
        if (result == null) {
            return;
        }
        if (Boolean.TRUE.equals(success)) {
            result.setResult(ResponseEntity.ok().build());
        } else {
            result.setResult(ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build());
        }

        redisTemplate.delete("status:" + operationId);
        redisTemplate.delete("softLock:priceChange:combination:" + productCombinationId);
    }

}
