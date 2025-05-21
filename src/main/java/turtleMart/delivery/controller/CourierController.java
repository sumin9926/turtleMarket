package turtleMart.delivery.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import turtleMart.delivery.dto.reqeust.CreateCourierRequest;
import turtleMart.delivery.dto.response.CreateCourierResponse;
import turtleMart.delivery.service.CourierService;

@RequestMapping
@RestController
@RequiredArgsConstructor
public class CourierController {

    private final CourierService courierService;

    @PostMapping("/api/couriers")
    public ResponseEntity<CreateCourierResponse> createCourier(@RequestBody CreateCourierRequest request) {
        CreateCourierResponse createCourierResponse = courierService.createCourier(request);

        return ResponseEntity.status(HttpStatus.CREATED).body(createCourierResponse);
    }
}
