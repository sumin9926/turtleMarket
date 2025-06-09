package turtleMart.global.slack;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import turtleMart.global.utill.JsonHelper;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

@Component
@RequiredArgsConstructor
public class SlackFormatter {

    private static final DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");

    // 재고 감소 실패 알림 메시지 생성
    public String formatInventoryDecreaseFailureMessage(Long orderId, String memberName, String productId, String productName, String reason, LocalDateTime time) {
        Map<String, Object> payload = getPayload();
        List<Object> blocks = new ArrayList<>();

        blocks.add(Map.of(
            "type", "header",
            "text", Map.of("type", "plain_text", "text", "❌ 재고 감소 실패 알림")
        ));

        blocks.add(Map.of(
            "type", "section",
            "fields", List.of(
                markdownField("*주문 ID:*", orderId.toString()),
                markdownField("*회원명:*", memberName)
            )
        ));

        blocks.add(Map.of(
            "type", "section",
            "fields", List.of(
                markdownField("*상품 ID:*", productId),
                markdownField("*상품명:*", productName)
            )
        ));

        blocks.add(Map.of(
            "type", "section",
            "text", markdownField("*\uD83D\uDD0D 사유:*", reason)
        ));

        blocks.add(Map.of(
            "type", "context",
            "elements", List.of(
                Map.of("type", "mrkdwn", "text", "\uD83D\uDD52 *시간:* " + time.format(formatter))
            )
        ));

        payload.put("blocks", blocks);

        return JsonHelper.toJson(payload);
    }

    // 배송 생성 성공 알림 메시지 생성
    public String formatDeliveryCreateMessage(Long orderId, String memberName, String phoneNumber, String receiver, String receiverPhone, String address, String detailAddress, LocalDateTime time) {
        Map<String, Object> payload = getPayload();
        List<Object> blocks = new ArrayList<>();

        blocks.add(Map.of(
            "type", "header",
            "text", Map.of("type", "plain_text", "text", "✅ 배송 생성 성공 알림")
        ));

        blocks.add(Map.of(
            "type", "section",
            "fields", List.of(
                markdownField("*주문 ID:*", orderId.toString())
            )
        ));

        blocks.add(Map.of(
            "type", "section",
            "fields", List.of(
                markdownField("*회원명:*", memberName),
                markdownField("*연락처:*", phoneNumber)
            )
        ));

        blocks.add(Map.of(
            "type", "section",
            "fields", List.of(
                markdownField("*수령인:*", receiver),
                markdownField("*수령인 연락처:*", receiverPhone),
                markdownField("*주소:*", address),
                markdownField("*상세주소:*", detailAddress)
            )
        ));

        blocks.add(Map.of(
            "type", "section",
            "fields", List.of(
                markdownField("*상태:*", "배송 생성 완료 ✅")
            )
        ));

        blocks.add(Map.of(
            "type", "context",
            "elements", List.of(
                Map.of("type", "mrkdwn", "text", "\uD83D\uDCE6 배송 정보가 정상적으로 등록되었습니다.\n"),
                Map.of("type", "mrkdwn", "text", "\uD83D\uDD52 *시간:* " + time.format(formatter))
            )
        ));

        payload.put("blocks", blocks);

        return JsonHelper.toJson(payload);
    }

    // 배송 생성 실패 알림 메시지 생성
    public String formatDeliveryCreateFailureMessage(Long orderId, String failureType, String reason, LocalDateTime time) {
        Map<String, Object> payload = getPayload();
        List<Object> blocks = new ArrayList<>();

        blocks.add(Map.of(
            "type", "header",
            "text", Map.of("type", "plain_text", "text", "❌ 배송 생성 실패 알림")
        ));

        blocks.add(Map.of(
            "type", "section",
            "text", markdownField("*주문 ID:*", orderId.toString())
        ));

        blocks.add(Map.of(
            "type", "section",
            "text", markdownField("*\uD83D\uDEAB 실패 유형:*", failureType)
        ));

        blocks.add(Map.of(
            "type", "section",
            "text", markdownField("*\uD83D\uDD0D 사유:*", reason)
        ));

        blocks.add(Map.of(
            "type", "context",
            "elements", List.of(
                Map.of("type", "mrkdwn", "text", "\uD83D\uDD52 *시간:* " + time.format(formatter))
            )
        ));

        payload.put("blocks", blocks);

        return JsonHelper.toJson(payload);
    }

    private Map<String, Object> getPayload() {
        return new LinkedHashMap<>();
    }

    // Block kit의 mrkdwn 필드 생성용
    private Map<String, Object> markdownField(String title, String value) {
        return Map.of("type", "mrkdwn", "text", title + "\n" + value);
    }
}
