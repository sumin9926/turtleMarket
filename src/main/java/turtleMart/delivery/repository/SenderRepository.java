package turtleMart.delivery.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import turtleMart.delivery.entity.Sender;

import java.util.List;

public interface SenderRepository extends JpaRepository<Sender, Long>, SenderQueryRepository {

    boolean existsByAddressAndDetailAddress(String address, String detailAddress);

    List<Sender> findAllByIsDeletedFalse();
}
