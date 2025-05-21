package turtleMart.delivery.service;

import lombok.RequiredArgsConstructor;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import turtleMart.delivery.dto.reqeust.CreateTrackingNumberRequest;
import turtleMart.delivery.entity.Courier;
import turtleMart.delivery.entity.TrackingNumber;
import turtleMart.delivery.repository.CourierRepository;
import turtleMart.delivery.repository.TrackingNumberRepository;
import turtleMart.global.exception.ErrorCode;
import turtleMart.global.exception.NotFoundException;

import java.util.UUID;

@Service
@RequiredArgsConstructor
@Transactional
public class TrackingNumberService {

    private final TrackingNumberRepository trackingNumberRepository;
    private final CourierRepository courierRepository;

    public String createTrackingNumber(CreateTrackingNumberRequest request) {
        Courier courier = courierRepository.findById(request.courierId())
            .orElseThrow(() -> new NotFoundException(ErrorCode.COURIER_NOT_FOUND));

        int retryCount = 0;
        final int MAX_RETRY = 3;

        while (retryCount < MAX_RETRY) {
            try {
                String trackingNumber = generateTrackingNumber(courier.getCode());

                trackingNumberRepository.save(TrackingNumber.of(trackingNumber));

                return trackingNumber;
            } catch (DataIntegrityViolationException e) {
                retryCount++;
                if (retryCount >= MAX_RETRY) {
                    throw new DataIntegrityViolationException("송장 번호 생성에 실패했습니다. 여러 번 시도했지만 저장하지 못했습니다.");
                }
            }
        }

        throw new RuntimeException("이 곳에 도달하면 안됩니다.");
    }

    private String generateTrackingNumber(String courierCode) {
        String trackingNumber = UUID.randomUUID().toString().replace("-", "").substring(0, 20).toUpperCase();
        return courierCode.toUpperCase() + trackingNumber;
    }
}
