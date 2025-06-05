package turtleMart.global.slack;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Component;
import org.springframework.web.reactive.function.client.WebClient;

import java.time.LocalDateTime;

@Slf4j
@Component
@RequiredArgsConstructor
public class SlackNotifier {

    private final WebClient webClient;
    private final SlackFormatter slackFormatter;

    @Value("${slack.webhook.url}")
    private String slackWebhookUrl;

    public void sendInventoryDecreaseFailureAlert(Long orderId, String memberName, String productId, String productName, String reason) {
        String message = slackFormatter.formatInventoryDecreaseFailureMessage(orderId, memberName, productId, productName, reason, LocalDateTime.now());

        sendPayload(message);
    }

    public void sendDeliveryCreateAlert(Long orderId, String memberName, String phoneNumber, String receiver, String receiverPhone, String address, String detailAddress) {
        String message = slackFormatter.formatDeliveryCreateMessage(orderId, memberName, phoneNumber, receiver, receiverPhone, address, detailAddress, LocalDateTime.now());

        sendPayload(message);
    }

    public void sendDeliveryCreateFailureAlert(Long orderId, String failureType, String reason) {
        String message = slackFormatter.formatDeliveryCreateFailureMessage(orderId, failureType, reason, LocalDateTime.now());

        sendPayload(message);
    }

    private void sendPayload(String payload) {
        webClient.post()
            .uri(slackWebhookUrl)
            .contentType(MediaType.APPLICATION_JSON)
            .bodyValue(payload)
            .retrieve()
            .bodyToMono(String.class)
            .doOnNext(response -> log.info("✅ Slack 응답: {}", response))
            .doOnError(e -> log.error("❌ Slack 전송 실패", e))
            .subscribe();
    }

    // 이스케이프 처리 (큰따옴표 등)
    private String escapeJson(String text) {
        return text.replace("\"", "\\\"");
    }
}