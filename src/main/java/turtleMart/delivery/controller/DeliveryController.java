package turtleMart.delivery.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import turtleMart.delivery.dto.reqeust.CreateDeliveryRequest;
import turtleMart.delivery.dto.reqeust.UpdateDeliveryRequest;
import turtleMart.delivery.dto.response.CreateDeliveryResponse;
import turtleMart.delivery.dto.response.UpdateDeliveryResponse;
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

    @PatchMapping("/{deliveryId}")
    public ResponseEntity<UpdateDeliveryResponse> updateTrackingNumber(
        @PathVariable(name = "deliveryId") Long deliveryId,
        @RequestBody UpdateDeliveryRequest request
    ) {
        UpdateDeliveryResponse updateDeliveryResponse = deliveryService.updateTrackingNumber(deliveryId, request);

        return ResponseEntity.status(HttpStatus.OK).body(updateDeliveryResponse);
    }
}
