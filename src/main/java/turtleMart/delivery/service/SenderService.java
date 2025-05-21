package turtleMart.delivery.service;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import turtleMart.delivery.dto.reqeust.CreateSenderRequest;
import turtleMart.delivery.dto.reqeust.UpdateSenderRequest;
import turtleMart.delivery.dto.response.SenderResponse;
import turtleMart.delivery.dto.response.UpdateSenderResponse;
import turtleMart.delivery.entity.Courier;
import turtleMart.delivery.entity.Sender;
import turtleMart.delivery.repository.CourierRepository;
import turtleMart.delivery.repository.SenderRepository;

import java.util.List;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class SenderService {

    private final SenderRepository senderRepository;
    private final CourierRepository courierRepository;

    @Transactional
    public SenderResponse createSender(CreateSenderRequest request) {
        if (senderRepository.existsByName(request.name())) {
            throw new RuntimeException("이미 존재하는 출고지(물류센터)입니다.");
        }

        Courier courier = courierRepository.findById(request.courierId())
            .orElseThrow(() -> new RuntimeException("존재하지 않는 택배사입니다."));

        Sender sender = Sender.of(courier, request.name(), request.phoneNumber(), request.address(), request.detailAddress());

        senderRepository.save(sender);

        return SenderResponse.from(sender);
    }

    public List<SenderResponse> readAllSenders() {
        List<Sender> senderList = senderRepository.findAllByIsDeletedFalse();

        return senderList.stream()
            .map(SenderResponse::from)
            .toList();
    }

    public SenderResponse readSender(Long senderId) {
        Sender sender = senderRepository.findById(senderId)
            .orElseThrow(() -> new RuntimeException("존재하지 않는 출고지(물류센터)입니다."));

        return SenderResponse.from(sender);
    }

    @Transactional
    public UpdateSenderResponse updateSender(UpdateSenderRequest request, Long senderId) {
        Sender sender = senderRepository.findById(senderId)
            .orElseThrow(() -> new RuntimeException("존재하지 않는 출고지(물류센터)입니다."));

        sender.update(request);

        return UpdateSenderResponse.from(sender);
    }
}
