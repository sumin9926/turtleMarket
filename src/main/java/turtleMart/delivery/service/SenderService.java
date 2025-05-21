package turtleMart.delivery.service;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import turtleMart.delivery.dto.reqeust.CreateSenderRequest;
import turtleMart.delivery.dto.response.CreateSenderResponse;
import turtleMart.delivery.entity.Courier;
import turtleMart.delivery.entity.Sender;
import turtleMart.delivery.repository.CourierRepository;
import turtleMart.delivery.repository.SenderRepository;

@Service
@RequiredArgsConstructor
public class SenderService {

    private final SenderRepository senderRepository;
    private final CourierRepository courierRepository;

    public CreateSenderResponse createSender(CreateSenderRequest request) {
        if (senderRepository.existsByName(request.name())) {
            throw new RuntimeException("이미 존재하는 출고지(물류센터)입니다.");
        }

        Courier courier = courierRepository.findById(request.courierId())
            .orElseThrow(() -> new RuntimeException("존재하는 택배사가 없습니다."));

        Sender sender = Sender.of(courier, request.name(), request.phoneNumber(), request.address(), request.detailAddress());

        senderRepository.save(sender);

        return CreateSenderResponse.from(sender);
    }
}
