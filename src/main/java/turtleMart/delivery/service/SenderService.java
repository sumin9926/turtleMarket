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
import turtleMart.global.exception.ConflictException;
import turtleMart.global.exception.ErrorCode;
import turtleMart.global.exception.NotFoundException;

import java.util.List;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class SenderService {

    private final SenderRepository senderRepository;
    private final CourierRepository courierRepository;

    @Transactional
    public SenderResponse createSender(CreateSenderRequest request) {
        if (senderRepository.existsByAddressAndDetailAddress(request.address(), request.detailAddress())) {
            throw new ConflictException(ErrorCode.SENDER_ALREADY_EXISTS);
        }

        if (!courierRepository.existsByIdAndIsDeletedFalse(request.courierId())) {
            throw new NotFoundException(ErrorCode.COURIER_NOT_FOUND);
        }

        Courier courier = courierRepository.getReferenceById(request.courierId());

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
        Sender sender = getSender(senderId);

        return SenderResponse.from(sender);
    }

    @Transactional
    public UpdateSenderResponse updateSender(UpdateSenderRequest request, Long senderId) {
        Sender sender = getSender(senderId);

        sender.update(request);

        return UpdateSenderResponse.from(sender);
    }

    @Transactional
    public void deleteSender(Long senderId) {
        Sender sender = getSender(senderId);

        sender.delete();
    }

    private Sender getSender(Long senderId) {
        return senderRepository.findById(senderId)
            .orElseThrow(() -> new NotFoundException(ErrorCode.SENDER_NOT_FOUND));
    }
}
