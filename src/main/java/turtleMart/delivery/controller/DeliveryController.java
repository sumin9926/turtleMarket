package turtleMart.delivery.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import turtleMart.delivery.dto.reqeust.CreateDeliveryRequest;
import turtleMart.delivery.dto.reqeust.UpdateDeliveryRequest;
import turtleMart.delivery.dto.response.CreateDeliveryResponse;
import turtleMart.delivery.dto.response.ReadDeliveryResponse;
import turtleMart.delivery.dto.response.UpdateDeliveryResponse;
import turtleMart.delivery.service.DeliveryService;

import java.util.List;

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

    @GetMapping("/{deliveryId}")
    public ResponseEntity<ReadDeliveryResponse> readDelivery(@PathVariable(name = "deliveryId") Long deliveryId) {
        ReadDeliveryResponse readDeliveryResponse = deliveryService.readDelivery(deliveryId);

        return ResponseEntity.status(HttpStatus.OK).body(readDeliveryResponse);
    }

    @GetMapping("/members/{memberId}")
    public ResponseEntity<List<ReadDeliveryResponse>> readAllDeliveriesByMember(
        @PathVariable(name = "memberId") Long memberId
    ){
        List<ReadDeliveryResponse> readDeliveryResponseList = deliveryService.readAllDeliveriesByMember(memberId);

        return ResponseEntity.status(HttpStatus.OK).body(readDeliveryResponseList);
    }

    @GetMapping("/sellers/{sellerId}")
    public ResponseEntity<List<ReadDeliveryResponse>> readAllDeliveriesBySeller(
        @PathVariable(name = "sellerId") Long sellerId
    ) {
        List<ReadDeliveryResponse> readDeliveryResponseList = deliveryService.readAllDeliveriesBySeller(sellerId);

        return ResponseEntity.status(HttpStatus.OK).body(readDeliveryResponseList);
    }
}
