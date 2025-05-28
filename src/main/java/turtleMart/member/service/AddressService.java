package turtleMart.member.service;

import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import turtleMart.delivery.repository.AddressRepository;
import turtleMart.member.dto.request.AddressRegisterRequest;
import turtleMart.member.dto.request.UpdateAddressRequest;
import turtleMart.member.dto.response.AddressResponse;
import turtleMart.member.entity.Address;

import java.util.List;

@Service
@RequiredArgsConstructor
public class AddressService {
    private final AddressRepository addressRepository;

    public AddressResponse registerAddress(AddressRegisterRequest request) {
        Address address = Address.of(request);
        Address savedAddress = addressRepository.save(address);
        return AddressResponse.from(savedAddress);
    }

    public AddressResponse getAddress(Long addressId) {
        Address foundAddress = findAddress(addressId);
        return AddressResponse.from(foundAddress);
    }

    public List<AddressResponse> getAddressList() {
        List<Address> addressList = addressRepository.findAll();
        return addressList.stream().map(AddressResponse::from).toList();
    }

    public AddressResponse modifyAddress(Long addressId, @Valid UpdateAddressRequest request) {
        Address foundAddress = findAddress(addressId);
        foundAddress.updateAddress(request);
        addressRepository.save(foundAddress);
        return AddressResponse.from(foundAddress);
    }

    public String deleteAddress(Long addressId) {
        Address foundAddress = findAddress(addressId);
        addressRepository.delete(foundAddress);
        return "주소가 삭제되었습니다.";
    }

    private Address findAddress(Long addressId) {
        return addressRepository.findById(addressId)
                .orElseThrow(() -> new RuntimeException("등록된 주소가 없습니다."));
    }
}
