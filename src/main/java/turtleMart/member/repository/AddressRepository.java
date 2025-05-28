package turtleMart.member.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import turtleMart.member.entity.Address;

public interface AddressRepository extends JpaRepository<Address, Long> {

}
