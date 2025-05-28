package turtleMart.member.conrtroller;

import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import turtleMart.member.dto.request.AddressRegisterRequest;
import turtleMart.member.dto.request.UpdateAddressRequest;
import turtleMart.member.dto.response.AddressResponse;
import turtleMart.member.service.AddressService;

import java.util.List;

@RestController
@RequestMapping("/members/userProfile/address")
@RequiredArgsConstructor
public class AddressController {
    private final AddressService addressService;

    @PostMapping("/register")
    public ResponseEntity<AddressResponse> registerAddress(
            @RequestBody @Valid AddressRegisterRequest request
    ) {
        AddressResponse addressResponse = addressService.registerAddress(request);
        return ResponseEntity.status(HttpStatus.CREATED).body(addressResponse);
    }

    @GetMapping("/myAddress/{addressId}")
    public ResponseEntity<AddressResponse> getMyAddress(
            @PathVariable Long addressId
    ) {
        AddressResponse foundMyAddress = addressService.getAddress(addressId);
        return ResponseEntity.status(HttpStatus.OK).body(foundMyAddress);
    }

    @GetMapping("/myAddress")
    public ResponseEntity<List<AddressResponse>> getMyAddressList() {
        List<AddressResponse> foundMyAddressList = addressService.getAddressList();
        return ResponseEntity.status(HttpStatus.OK).body(foundMyAddressList);
    }

    @PatchMapping("/myAddress/{addressId}/modify")
    public ResponseEntity<AddressResponse> modifyMyAddress(
            @PathVariable Long addressId,
            @RequestBody @Valid UpdateAddressRequest request
    ) {
        AddressResponse addressResponse = addressService.modifyAddress(addressId, request);
        return ResponseEntity.status(HttpStatus.OK).body(addressResponse);
    }

    @DeleteMapping("/myAddress/{addressId}")
    public ResponseEntity<String> deleteMyAddress(
            @PathVariable Long addressId
    ) {
        String resultMessage = addressService.deleteAddress(addressId);
        return ResponseEntity.status(HttpStatus.OK).body(resultMessage);
    }
}
