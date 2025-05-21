package turtleMart.delivery.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import turtleMart.delivery.dto.reqeust.CreateCourierRequest;
import turtleMart.delivery.dto.reqeust.UpdateCourierRequest;
import turtleMart.delivery.dto.response.CreateCourierResponse;
import turtleMart.delivery.dto.response.ReadCourierResponse;
import turtleMart.delivery.dto.response.UpdateCourierResponse;
import turtleMart.delivery.service.CourierService;

import java.util.List;

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

    @GetMapping("/api/couriers")
    public ResponseEntity<List<ReadCourierResponse>> readAllCouriers() {
        List<ReadCourierResponse> readCourierResponseList = courierService.readAllCouriers();

        return ResponseEntity.status(HttpStatus.OK).body(readCourierResponseList);
    }

    @PatchMapping("/api/couriers/{courierId}")
    public ResponseEntity<UpdateCourierResponse> updateCourier(
        @RequestBody UpdateCourierRequest request,
        @PathVariable(name = "courierId") Long courierId
    ) {
        UpdateCourierResponse updateCourierResponse = courierService.updateCourier(request, courierId);

        return ResponseEntity.status(HttpStatus.OK).body(updateCourierResponse);
    }
}
