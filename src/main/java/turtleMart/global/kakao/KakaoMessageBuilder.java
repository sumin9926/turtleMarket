package turtleMart.global.kakao;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import turtleMart.global.kakao.dto.UserNotification;

@Slf4j
@Component
public class KakaoMessageBuilder {

    public String buildTextTemplateObject(UserNotification userNotification) {
        try {
            ObjectMapper objectMapper = new ObjectMapper();

            // 텍스트 블록 구성
            String text = """
                [TurtleMart 상품 출고 안내]
                고객님, 주문하신 상품이 안전하게 출고되었습니다!
                
                택배사로 상품이 전달되었으며,
                배송 조회까지 평일 기준 1~2일 정도 소요될 수 있습니다.
                상품 수령까지 조금만 기다려주세요!
                
                ■ 주문 정보
                주문번호: %s
                
                ■ 배송 정보
                택배사: %s
                송장번호: %s
                
                ■ 참고사항
                - 정해진 출고지(%s)에서 출고됩니다.
                
                감사합니다. 좋은 하루 보내세요 😊
                """.formatted(
                userNotification.orderId(),
                userNotification.courierName(),
                userNotification.trackingNumber(),
                userNotification.senderName()
            );

            ObjectNode root = objectMapper.createObjectNode();
            root.put("object_type", "text");
            root.put("text", text);

            String trackingUrl = userNotification.trackingUrlTemplate()
                .replace("{trackingNumber}", userNotification.trackingNumber());

            ObjectNode link = objectMapper.createObjectNode();
            link.put("web_url", trackingUrl);
            link.put("mobile_web_url", trackingUrl);

            root.set("link", link);
            root.put("button_title", "배송 조회");

            return objectMapper.writeValueAsString(root);
        } catch (Exception e) {
            log.error("❌ 카카오 출고 완료 템플릿 생성 실패: {}", e.getMessage());
            return null;
        }
    }

    public String buildTextInTransitTemplateObject(UserNotification userNotification) {
        try {
            ObjectMapper objectMapper = new ObjectMapper();

            // 텍스트 블록 구성
            String text = """
                [TurtleMart 상품 배송 안내]
                고객님, 주문하신 상품이 현재 배송 중입니다.
                
                📦 상품 수령까지 조금만 기다려주세요!
                배송 조회는 하단 버튼을 통해 가능합니다.
                
                ■ 주문 정보
                주문번호: %s
                
                ■ 배송 정보
                택배사: %s
                송장번호: %s
                
                ■ 수령인 정보
                수령인: %s,
                주소: %s
                """.formatted(
                userNotification.orderId(),
                userNotification.courierName(),
                userNotification.trackingNumber(),
                userNotification.receiverName(),
                userNotification.receiverAddress()
            );

            ObjectNode root = objectMapper.createObjectNode();
            root.put("object_type", "text");
            root.put("text", text);

            String trackingUrl = userNotification.trackingUrlTemplate()
                .replace("{trackingNumber}", userNotification.trackingNumber());

            ObjectNode link = objectMapper.createObjectNode();
            link.put("web_url", trackingUrl);
            link.put("mobile_web_url", trackingUrl);

            root.set("link", link);
            root.put("button_title", "배송 조회");

            return objectMapper.writeValueAsString(root);
        } catch (Exception e) {
            log.error("❌ 카카오 배송 중 템플릿 생성 실패: {}", e.getMessage());
            return null;
        }
    }

    public String buildTextDeliveredTemplateObject(UserNotification userNotification) {
        try {
            ObjectMapper objectMapper = new ObjectMapper();

            // 텍스트 블록 구성
            String text = """
                [TurtleMart 상품 배송 완료 안내]
                고객님, 주문하신 상품이 안전하게 배송 완료되었습니다.
                
                상품 수령에 이상이 없으신지 확인 부탁드리며,
                소중한 이용에 진심으로 감사드립니다 😊
                
                ■ 주문 정보
                주문번호: %s
                
                ■ 배송 정보
                택배사: %s
                송장번호: %s
                
                ■ 수령인 정보
                수령인: %s,
                주소: %s
                
                더 나은 서비스로 보답하겠습니다. 감사합니다!
                """.formatted(
                userNotification.orderId(),
                userNotification.courierName(),
                userNotification.trackingNumber(),
                userNotification.receiverName(),
                userNotification.receiverAddress()
            );

            ObjectNode root = objectMapper.createObjectNode();
            root.put("object_type", "text");
            root.put("text", text);

            String trackingUrl = userNotification.trackingUrlTemplate()
                .replace("{trackingNumber}", userNotification.trackingNumber());

            ObjectNode link = objectMapper.createObjectNode();
            link.put("web_url", trackingUrl);
            link.put("mobile_web_url", trackingUrl);

            root.set("link", link);
            root.put("button_title", "주문 조회");

            return objectMapper.writeValueAsString(root);
        } catch (Exception e) {
            log.error("❌ 카카오 배송 완료 템플릿 생성 실패: {}", e.getMessage());
            return null;
        }
    }
}
