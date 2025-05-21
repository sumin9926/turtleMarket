package turtleMart.delivery.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import turtleMart.delivery.dto.reqeust.CreateDeliveryRequest;
import turtleMart.delivery.dto.response.CreateDeliveryResponse;
import turtleMart.delivery.service.DeliveryService;

@RequestMapping("/api/deliveries")
@RestController
@RequiredArgsConstructor
public class DeliveryController {

    private final DeliveryService deliveryService;

    @PostMapping
    public ResponseEntity<CreateDeliveryResponse> createDelivery(@RequestBody CreateDeliveryRequest request) {
        CreateDeliveryResponse createDeliveryResponse = deliveryService.createDelivery(request);

        return ResponseEntity.status(HttpStatus.CREATED).body(createDeliveryResponse);
    }
}
