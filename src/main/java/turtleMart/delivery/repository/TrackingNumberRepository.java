package turtleMart.delivery.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import turtleMart.delivery.entity.TrackingNumber;

public interface TrackingNumberRepository extends JpaRepository<TrackingNumber, String> {
}
