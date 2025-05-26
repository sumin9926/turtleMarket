package turtleMart.member.conrtroller;

import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import turtleMart.member.dto.request.AddressRegisterRequest;
import turtleMart.member.dto.response.AddressResponse;
import turtleMart.member.service.AddressService;

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
}
