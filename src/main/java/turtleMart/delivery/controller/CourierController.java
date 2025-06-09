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

@RequestMapping("/couriers")
@RestController
@RequiredArgsConstructor
public class CourierController {

    private final CourierService courierService;

    @PostMapping
    public ResponseEntity<CreateCourierResponse> createCourier(@RequestBody CreateCourierRequest request) {
        CreateCourierResponse createCourierResponse = courierService.createCourier(request);

        return ResponseEntity.status(HttpStatus.CREATED).body(createCourierResponse);
    }

    @GetMapping
    public ResponseEntity<List<ReadCourierResponse>> readAllCouriers() {
        List<ReadCourierResponse> readCourierResponseList = courierService.readAllCouriers();

        return ResponseEntity.status(HttpStatus.OK).body(readCourierResponseList);
    }

    @PatchMapping("/{courierId}")
    public ResponseEntity<UpdateCourierResponse> updateCourier(
        @RequestBody UpdateCourierRequest request,
        @PathVariable(name = "courierId") Long courierId
    ) {
        UpdateCourierResponse updateCourierResponse = courierService.updateCourier(request, courierId);

        return ResponseEntity.status(HttpStatus.OK).body(updateCourierResponse);
    }

    @DeleteMapping("/{courierId}")
    public ResponseEntity<Void> deleteCourier(@PathVariable(name = "courierId") Long courierId) {
        courierService.deleteCourier(courierId);

        return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
    }
}
