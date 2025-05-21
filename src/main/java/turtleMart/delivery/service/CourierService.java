package turtleMart.delivery.service;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import turtleMart.delivery.dto.reqeust.CreateCourierRequest;
import turtleMart.delivery.dto.reqeust.UpdateCourierRequest;
import turtleMart.delivery.dto.response.CreateCourierResponse;
import turtleMart.delivery.dto.response.ReadCourierResponse;
import turtleMart.delivery.dto.response.UpdateCourierResponse;
import turtleMart.delivery.entity.Courier;
import turtleMart.delivery.repository.CourierRepository;
import turtleMart.delivery.repository.SenderRepository;

import java.util.List;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class CourierService {

    private final CourierRepository courierRepository;
    private final SenderRepository senderRepository;

    @Transactional
    public CreateCourierResponse createCourier(CreateCourierRequest request) {
        if (courierRepository.existsByNameAndCode(request.name(), request.code())) {
            throw new RuntimeException("이미 존재하는 택배사입니다.");
        }

        Courier courier = Courier.of(request.name(), request.code(), request.trackingUrlTemplate());

        courierRepository.save(courier);

        return CreateCourierResponse.from(courier);
    }

    public List<ReadCourierResponse> readAllCouriers() {
        List<Courier> courierList = courierRepository.findAllByIsDeletedFalse();

        return courierList.stream()
            .map(ReadCourierResponse::from)
            .toList();
    }

    @Transactional
    public UpdateCourierResponse updateCourier(UpdateCourierRequest request, Long courierId) {
        Courier courier = courierRepository.findById(courierId)
            .orElseThrow(() -> new RuntimeException("존재하지 않는 택배사입니다."));

        courier.update(request);

        return UpdateCourierResponse.from(courier);
    }

    @Transactional
    public void deleteCourier(Long courierId) {
        Courier courier = courierRepository.findById(courierId)
            .orElseThrow(() -> new RuntimeException("존재하지 않는 택배사입니다."));

        long senderCount = senderRepository.countByCourierId(courier);

        if (senderCount != 0) {
            throw new RuntimeException("계약된 출고지(물류센터)가 존재하기 때문에 삭제할 수 없습니다.");
        }

        courier.delete();
    }
}
