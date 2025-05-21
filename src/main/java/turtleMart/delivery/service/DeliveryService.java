package turtleMart.delivery.service;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import turtleMart.delivery.dto.reqeust.CreateDeliveryRequest;
import turtleMart.delivery.dto.reqeust.UpdateDeliveryRequest;
import turtleMart.delivery.dto.response.CreateDeliveryResponse;
import turtleMart.delivery.dto.response.ReadDeliveryResponse;
import turtleMart.delivery.dto.response.UpdateDeliveryResponse;
import turtleMart.delivery.entity.Delivery;
import turtleMart.delivery.entity.Sender;
import turtleMart.delivery.repository.DeliveryRepository;
import turtleMart.delivery.repository.SenderRepository;
import turtleMart.global.exception.ErrorCode;
import turtleMart.global.exception.NotFoundException;
import turtleMart.member.entity.Address;
import turtleMart.member.entity.Seller;
import turtleMart.member.repository.AddressRepository;
import turtleMart.member.repository.SellerRepository;
import turtleMart.order.entity.Order;
import turtleMart.order.repository.OrderRepository;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class DeliveryService {

    private final DeliveryRepository deliveryRepository;
    private final OrderRepository orderRepository;
    private final SellerRepository sellerRepository;
    private final SenderRepository senderRepository;
    private final AddressRepository addressRepository;

    @Transactional
    public CreateDeliveryResponse createDelivery(CreateDeliveryRequest request) {
        if (!orderRepository.existsById(request.orderId())) {
            throw new RuntimeException("존재하지 않는 주문입니다.");
        }
        Order order = orderRepository.getReferenceById(request.orderId());

        if (!sellerRepository.existsById(request.sellerId())) {
            throw new RuntimeException("존재하지 않는 판매자입니다.");
        }
        Seller seller = sellerRepository.getReferenceById(request.sellerId());

        if (!senderRepository.existsById(request.senderId())) {
            throw new RuntimeException("존재하지 않는 출고지(물류센터)입니다.");
        }
        Sender sender = senderRepository.getReferenceById(request.senderId());

        Address address = addressRepository.findById(request.addressId())
            .orElseThrow(() -> new NotFoundException(ErrorCode.SENDER_NOT_FOUND));

        Delivery delivery = Delivery.of(order, seller, sender, address, request.deliveryRequest());

        deliveryRepository.save(delivery);

        return CreateDeliveryResponse.from(delivery);
    }

    @Transactional
    public UpdateDeliveryResponse updateTrackingNumber(Long deliveryId, UpdateDeliveryRequest request) {
        Delivery delivery = deliveryRepository.findById(deliveryId)
            .orElseThrow(() -> new NotFoundException(ErrorCode.DELIVERY_NOT_FOUND));

        delivery.updateTrackingNumber(request.trackingNumber());

        return UpdateDeliveryResponse.from(delivery);
    }

    public ReadDeliveryResponse readDelivery(Long deliveryId) {
        Delivery delivery = deliveryRepository.findById(deliveryId)
            .orElseThrow(() -> new NotFoundException(ErrorCode.DELIVERY_NOT_FOUND));

        return ReadDeliveryResponse.from(delivery);
    }
}
