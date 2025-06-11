package turtleMart.payment.controller;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;
import lombok.RequiredArgsConstructor;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.stereotype.Controller;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.ui.Model;
import turtleMart.delivery.dto.reqeust.CreateDeliveryRequest;
import turtleMart.global.exception.ErrorCode;
import turtleMart.global.exception.NotFoundException;
import turtleMart.global.kafka.dto.OperationWrapperDto;
import turtleMart.global.kafka.enums.OperationType;
import turtleMart.global.utill.JsonHelper;
import turtleMart.order.dto.request.OrderWrapperRequest;
import turtleMart.order.entity.Order;
import turtleMart.order.repository.OrderRepository;
import turtleMart.payment.dto.PaymentInfoTransfer;
import turtleMart.payment.service.EmailService;
import turtleMart.payment.service.PaymentService;

import java.io.*;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.Base64;

@Controller
@RequiredArgsConstructor
public class WidgetController {
    private final OrderRepository orderRepository;
    private final EmailService emailService;
    private final PaymentService paymentService;
    private final KafkaTemplate<String, String> stringKafkaTemplate;
    private final Logger logger = LoggerFactory.getLogger(this.getClass());

    /**
     * POST /checkout에서 Order, payment 객체 만들고, GET /checkout 리다이렉트 하면서 결제 로직 시작.<br>
     * 세션에 배송 관련 정보 담아뒀다가 /confirm에서 꺼내서 카프카로 이벤트 발행
     */
    //TODO : /checkout 메서드 안에 수민님 Order 로직 넣기
    @PostMapping("/checkout")
    public String createAndRedirect(
            @RequestBody OrderWrapperRequest dto,
            @RequestParam Long orderId,
            HttpSession session){

        paymentService.createPayment(dto.payment());
        session.setAttribute("delivery", dto.delivery());
        return "redirect:/checkout?orderId=" + orderId;
    }

    @GetMapping("/checkout")
    public String checkoutPage(@RequestParam Long orderId, Model model) throws Exception {
        Order order = orderRepository.findById(orderId).orElseThrow(
                () -> new NotFoundException(ErrorCode.ORDER_NOT_FOUND));
        PaymentInfoTransfer dto = PaymentInfoTransfer.from(order);
        model.addAttribute("order", dto);
        return "checkout";
    }

    /**
     * 인증성공처리
     * @param request
     * @param model
     * @return
     * @throws Exception
     */
    @GetMapping("/success")
    public String paymentRequest(HttpServletRequest request, Model model) throws Exception {
        return "success";
    }

    /**
     * 인증실패처리
     * @param request
     * @param model
     * @return
     * @throws Exception
     */
    @GetMapping("/fail")
    public String failPayment(HttpServletRequest request, Model model) throws Exception {
        String failCode = request.getParameter("code");
        String failMessage = request.getParameter("message");

        model.addAttribute("code", failCode);
        model.addAttribute("message", failMessage);

        return "fail";
    }


    @RequestMapping(value = "/confirm")
    public ResponseEntity<JSONObject> confirmPayment(@RequestBody String jsonBody, HttpSession session) throws Exception {

        JSONParser parser = new JSONParser();
        String orderId;
        String amount;
        String paymentKey;
        try {
            // 클라이언트에서 받은 JSON 요청 바디입니다.
            JSONObject requestData = (JSONObject) parser.parse(jsonBody);
            paymentKey = (String) requestData.get("paymentKey");
            orderId = (String) requestData.get("orderId");
            amount = (String) requestData.get("amount");
        } catch (ParseException e) {
            throw new RuntimeException(e);
        }

        JSONObject obj = new JSONObject();
        obj.put("orderId", orderId);
        obj.put("amount", amount);
        obj.put("paymentKey", paymentKey);

        // 토스페이먼츠 API는 시크릿 키를 사용자 ID로 사용하고, 비밀번호는 사용하지 않습니다.
        // 비밀번호가 없다는 것을 알리기 위해 시크릿 키 뒤에 콜론을 추가합니다.
        String widgetSecretKey = "test_gsk_docs_OaPz8L5KdmQXkzRz3y47BMw6";
        Base64.Encoder encoder = Base64.getEncoder();
        byte[] encodedBytes = encoder.encode((widgetSecretKey + ":").getBytes(StandardCharsets.UTF_8));
        String authorizations = "Basic " + new String(encodedBytes);

        // 결제를 승인하면 결제수단에서 금액이 차감돼요.
        URL url = new URL("https://api.tosspayments.com/v1/payments/confirm");
        HttpURLConnection connection = (HttpURLConnection) url.openConnection();
        connection.setRequestProperty("Authorization", authorizations);
        connection.setRequestProperty("Content-Type", "application/json");
        connection.setRequestMethod("POST");
        connection.setDoOutput(true);

        OutputStream outputStream = connection.getOutputStream();
        outputStream.write(obj.toString().getBytes("UTF-8"));

        int code = connection.getResponseCode();
        boolean isSuccess = code == 200;

        InputStream responseStream = isSuccess ? connection.getInputStream() : connection.getErrorStream();

        // 결제 성공 및 실패 비즈니스 로직을 구현하세요.
        Reader reader = new InputStreamReader(responseStream, StandardCharsets.UTF_8);
        JSONObject jsonObject = (JSONObject) parser.parse(reader);
        responseStream.close();

        Long rawOrderId = Long.parseLong(orderId.substring(6));
        emailService.sendPaymentCompleteEmail(rawOrderId);

        CreateDeliveryRequest request = (CreateDeliveryRequest) session.getAttribute("delivery");
        String payload = JsonHelper.toJson(request);
        String key = orderId;
        String value = JsonHelper.toJson(OperationWrapperDto.from(OperationType.ORDER_PAYMENT_INVENTORY_DECREASE, payload));

        stringKafkaTemplate.send("${kafka.topic.product}", key, value);

        return ResponseEntity.status(code).body(jsonObject);
    }

    //    @GetMapping("/")
    //    public String index(HttpServletRequest request, Model model) throws Exception {
    //        return "/checkout";
    //    }
}
