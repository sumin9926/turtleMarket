package turtleMart.delivery.service;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import turtleMart.delivery.dto.reqeust.CreateCourierRequest;
import turtleMart.delivery.dto.response.CreateCourierResponse;
import turtleMart.delivery.entity.Courier;
import turtleMart.delivery.repository.CourierRepository;

@Service
@RequiredArgsConstructor
public class CourierService {

    private final CourierRepository courierRepository;

    public CreateCourierResponse createCourier(CreateCourierRequest request) {
        if(courierRepository.existsByNameAndCode(request.name(), request.code())) {
            throw new IllegalArgumentException("이미 존재하는 택배사입니다.");
        }

        Courier courier = Courier.of(request.name(), request.code(), request.trackingUrlTemplate());

        courierRepository.save(courier);

        return CreateCourierResponse.from(courier);
    }

}
