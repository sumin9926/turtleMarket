package turtleMart.delivery.service;

import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import turtleMart.delivery.dto.reqeust.CreateDeliveryRequest;
import turtleMart.delivery.dto.reqeust.UpdateDeliveryRequest;
import turtleMart.delivery.dto.reqeust.UpdateDeliveryStatusRequest;
import turtleMart.delivery.dto.response.CreateDeliveryResponse;
import turtleMart.delivery.dto.response.ReadDeliveryResponse;
import turtleMart.delivery.dto.response.UpdateDeliveryResponse;
import turtleMart.delivery.entity.Delivery;
import turtleMart.delivery.entity.DeliveryStatus;
import turtleMart.delivery.entity.Sender;
import turtleMart.delivery.repository.DeliveryRepository;
import turtleMart.delivery.repository.SenderRepository;
import turtleMart.global.exception.BadRequestException;
import turtleMart.global.exception.ConflictException;
import turtleMart.global.exception.ErrorCode;
import turtleMart.global.exception.NotFoundException;
import turtleMart.global.kafka.dto.OperationWrapperDto;
import turtleMart.global.kafka.enums.OperationType;
import turtleMart.global.kakao.KakaoMessageService;
import turtleMart.global.kakao.dto.UserNotification;
import turtleMart.global.slack.SlackNotifier;
import turtleMart.global.utill.JsonHelper;
import turtleMart.member.entity.Address;
import turtleMart.member.entity.Seller;
import turtleMart.member.repository.AddressRepository;
import turtleMart.member.repository.SellerRepository;
import turtleMart.order.entity.Order;
import turtleMart.order.repository.OrderRepository;

import java.util.List;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class DeliveryService {

    private final DeliveryRepository deliveryRepository;
    private final OrderRepository orderRepository;
    private final SellerRepository sellerRepository;
    private final SenderRepository senderRepository;
    private final AddressRepository addressRepository;
    private final SlackNotifier slackNotifier;
    private final KakaoMessageService kakaoMessageService;
    private final KafkaTemplate<String, Object> kafkaTemplate;

    @Value("${kafka.topic.product}")
    private String productTopic;

    @Transactional
    public CreateDeliveryResponse createDelivery(CreateDeliveryRequest request) {
        if (!orderRepository.existsById(request.orderId())) {
            String payload = JsonHelper.toJson(request);
            OperationWrapperDto wrapper = OperationWrapperDto.from(OperationType.DELIVERY_FAIL_INVENTORY_RESTORE, payload);
            String wrappedMessage = JsonHelper.toJson(wrapper);

            kafkaTemplate.send(productTopic, request.orderId().toString(), wrappedMessage);

            slackNotifier.sendDeliveryCreateFailureAlert(
                request.orderId(),
                "ORDER_NOT_FOUND",
                "ID가 " + request.orderId() + "인 주문이 존재하지 않아 배송 생성에 실패했습니다.");

            throw new NotFoundException(ErrorCode.ORDER_NOT_FOUND);
        }

        // 해당 주문에 대한 배송이 존재할 경우 예외 처리
        if (deliveryRepository.existsByOrderId(request.orderId())) {
            String payload = JsonHelper.toJson(request);
            OperationWrapperDto wrapper = OperationWrapperDto.from(OperationType.DELIVERY_FAIL_INVENTORY_RESTORE, payload);
            String wrappedMessage = JsonHelper.toJson(wrapper);

            kafkaTemplate.send(productTopic, request.orderId().toString(), wrappedMessage);

            slackNotifier.sendDeliveryCreateFailureAlert(
                request.orderId(),
                "DELIVERY_ALREADY_EXISTS",
                "ID가 " + request.orderId() + "인 주문에 대한 배송이 이미 존재하여 배송 생성에 실패했습니다.");

            throw new ConflictException(ErrorCode.DELIVERY_ALREADY_EXISTS);
        }

        if (!sellerRepository.existsById(request.sellerId())) {
            String payload = JsonHelper.toJson(request);
            OperationWrapperDto wrapper = OperationWrapperDto.from(OperationType.DELIVERY_FAIL_INVENTORY_RESTORE, payload);
            String wrappedMessage = JsonHelper.toJson(wrapper);

            kafkaTemplate.send(productTopic, request.orderId().toString(), wrappedMessage);

            slackNotifier.sendDeliveryCreateFailureAlert(
                request.orderId(),
                "SELLER_NOT_FOUND",
                "ID가 " + request.sellerId() + "인 판매자가 존재하지 않아 배송 생성에 실패했습니다.");

            throw new NotFoundException(ErrorCode.SELLER_NOT_FOUND);
        }

        if (!senderRepository.existsById(request.senderId())) {
            String payload = JsonHelper.toJson(request);
            OperationWrapperDto wrapper = OperationWrapperDto.from(OperationType.DELIVERY_FAIL_INVENTORY_RESTORE, payload);
            String wrappedMessage = JsonHelper.toJson(wrapper);

            kafkaTemplate.send(productTopic, request.orderId().toString(), wrappedMessage);

            slackNotifier.sendDeliveryCreateFailureAlert(
                request.orderId(),
                "SENDER_NOT_FOUND",
                "ID가 " + request.senderId() + "인 출고지(물류센터)가 존재하지 않아 배송 생성에 실패했습니다.");

            throw new NotFoundException(ErrorCode.SENDER_NOT_FOUND);
        }

        if (!addressRepository.existsById(request.addressId())) {
            String payload = JsonHelper.toJson(request);
            OperationWrapperDto wrapper = OperationWrapperDto.from(OperationType.DELIVERY_FAIL_INVENTORY_RESTORE, payload);
            String wrappedMessage = JsonHelper.toJson(wrapper);

            kafkaTemplate.send(productTopic, request.orderId().toString(), wrappedMessage);

            slackNotifier.sendDeliveryCreateFailureAlert(
                request.orderId(),
                "ADDRESS_NOT_FOUND",
                "ID가 " + request.addressId() + "인 주소가 존재하지 않아 배송 생성에 실패했습니다.");

            throw new NotFoundException(ErrorCode.ADDRESS_NOT_FOUND);
        }

        Order order = orderRepository.getReferenceById(request.orderId());
        Seller seller = sellerRepository.getReferenceById(request.sellerId());
        Sender sender = senderRepository.getReferenceById(request.senderId());

        Address address = addressRepository.findById(request.addressId())
            .orElseThrow(() -> new NotFoundException(ErrorCode.ADDRESS_NOT_FOUND));

        Delivery delivery = Delivery.of(order, seller, sender, address, request.deliveryRequest());

        deliveryRepository.save(delivery);

        // 슬랙 알림 메시지 전송
        slackNotifier.sendDeliveryCreateAlert(
            request.orderId(),
            delivery.getOrder().getMember().getName(),
            delivery.getOrder().getMember().getPhoneNumber(),
            delivery.getReceiverName(),
            delivery.getReceiverPhone(),
            delivery.getReceiverAddress(),
            delivery.getReceiverDetailAddress());

        return CreateDeliveryResponse.from(delivery);
    }

    @Transactional
    public UpdateDeliveryResponse updateTrackingNumber(Long deliveryId, UpdateDeliveryRequest request) {
        Delivery delivery = getDelivery(deliveryId);

        delivery.updateTrackingNumber(request.trackingNumber());

        // 카카오톡 알림 메시지 전송
        UserNotification userNotification = UserNotification.from(delivery);
        kakaoMessageService.sendMessage(userNotification);

        return UpdateDeliveryResponse.from(delivery);
    }

    public ReadDeliveryResponse readDelivery(Long deliveryId) {
        Delivery delivery = getDelivery(deliveryId);

        return ReadDeliveryResponse.from(delivery);
    }

    public List<ReadDeliveryResponse> readAllDeliveriesByMember(Long memberId) {
        List<Delivery> deliveryList = deliveryRepository.findAllByMemberId(memberId);

        return deliveryList.stream()
            .map(ReadDeliveryResponse::from)
            .toList();
    }

    public List<ReadDeliveryResponse> readAllDeliveriesBySeller(Long sellerId) {
        List<Delivery> deliveryList = deliveryRepository.findAllBySeller(sellerId);

        return deliveryList.stream()
            .map(ReadDeliveryResponse::from)
            .toList();
    }

    @Transactional
    public UpdateDeliveryResponse updateDeliveryStatus(Long deliveryId, UpdateDeliveryStatusRequest request) {
        Delivery delivery = getDelivery(deliveryId);

        if (!delivery.getDeliveryStatus().canTransitionTo(request.deliveryStatus())) {
            throw new BadRequestException(ErrorCode.INVALID_DELIVERY_STATUS);
        }

        if (request.deliveryStatus() == DeliveryStatus.DELIVERED) {
            delivery.updateDelivered(request.deliveryStatus());
        }

        delivery.updateDeliveryStatus(request.deliveryStatus());

        return UpdateDeliveryResponse.from(delivery);
    }

    private Delivery getDelivery(Long deliveryId) {
        return deliveryRepository.findById(deliveryId)
            .orElseThrow(() -> new NotFoundException(ErrorCode.DELIVERY_NOT_FOUND));
    }
}
