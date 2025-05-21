package turtleMart.member.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import turtleMart.member.entity.Seller;

public interface SellerRepository extends JpaRepository<Seller, Long> {

}
