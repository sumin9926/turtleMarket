package turtleMart.global.component;

import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import org.springframework.web.context.request.async.DeferredResult;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

@Component
public class DeferredResultStore {

    private final Map<String, DeferredResult<ResponseEntity<?>>> deferredResultMap = new ConcurrentHashMap<>();

    public void put(String key, DeferredResult<ResponseEntity<?>> result) {
        deferredResultMap.put(key, result);
    }

    public DeferredResult<ResponseEntity<?>> get(String key) {
        return deferredResultMap.get(key);
    }

    public DeferredResult<ResponseEntity<?>> remove(String key) {
        return deferredResultMap.remove(key);
    }
}
