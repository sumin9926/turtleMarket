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

import java.util.List;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class CourierService {

    private final CourierRepository courierRepository;

    @Transactional
    public CreateCourierResponse createCourier(CreateCourierRequest request) {
        if (courierRepository.existsByNameAndCode(request.name(), request.code())) {
            throw new IllegalArgumentException("이미 존재하는 택배사입니다.");
        }

        Courier courier = Courier.of(request.name(), request.code(), request.trackingUrlTemplate());

        courierRepository.save(courier);

        return CreateCourierResponse.from(courier);
    }

    public List<ReadCourierResponse> readAllCouriers() {
        List<Courier> courierList = courierRepository.findAll();

        return courierList.stream()
            .map(ReadCourierResponse::from)
            .toList();
    }

    @Transactional
    public UpdateCourierResponse updateCourier(UpdateCourierRequest request, Long courierId) {
        Courier courier = courierRepository.findById(courierId)
            .orElseThrow(() -> new IllegalArgumentException("존재하지 않는 택배사입니다."));

        courier.update(request);

        return UpdateCourierResponse.from(courier);
    }
}
