package turtleMart.member.service;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import turtleMart.delivery.repository.AddressRepository;
import turtleMart.member.dto.request.AddressRegisterRequest;
import turtleMart.member.dto.response.AddressResponse;
import turtleMart.member.entity.Address;

@Service
@RequiredArgsConstructor
public class AddressService {
    private final AddressRepository addressRepository;

    public AddressResponse registerAddress(AddressRegisterRequest request) {
        Address address = Address.of(request);
        Address savedAddress = addressRepository.save(address);
        return AddressResponse.from(savedAddress);
    }
}
