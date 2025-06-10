package turtleMart.global.kakao;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpHeaders;
import org.springframework.stereotype.Service;
import org.springframework.web.reactive.function.BodyInserters;
import org.springframework.web.reactive.function.client.WebClient;
import org.springframework.web.reactive.function.client.WebClientResponseException;
import turtleMart.global.kakao.dto.UserNotification;

@Slf4j
@Service
@RequiredArgsConstructor
public class KakaoMessageService {

    private final KakaoMessageBuilder kakaoMessageBuilder;
    private final WebClient webClient;

    @Value("${kakao.token}")
    private String token;

    @Value("${kakao.url}")
    private String url;

    public void sendMessage(UserNotification userNotification) {
        String templateObject = kakaoMessageBuilder.buildTextTemplateObject(userNotification);

        if (templateObject == null || templateObject.isBlank()) {
            log.warn("⚠️ 템플릿 생성 실패로 메시지 전송이 중단되었습니다.");
            return;
        }

        webClient.post()
            .uri(url)
            .header(HttpHeaders.AUTHORIZATION, "Bearer " + token)
            .body(BodyInserters.fromFormData("template_object", templateObject))
            .retrieve()
            .bodyToMono(String.class)
            .doOnNext(response -> log.info("✅ 카카오 메시지 전송 성공: {}", response))
            .subscribe(
                response -> log.info("✅ 카카오 응답 성공: {}", response),
                error -> {
                    if (error instanceof WebClientResponseException e) {
                        log.warn("❌ 카카오 API 응답 실패: {} - {}", e.getStatusCode(), e.getResponseBodyAsString());
                    } else {
                        log.error("❌ 카카오 메시지 전송 중 알 수 없는 오류 발생", error);
                    }
                }
            );
    }
}
