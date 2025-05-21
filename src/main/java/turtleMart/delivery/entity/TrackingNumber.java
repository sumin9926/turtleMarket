package turtleMart.delivery.entity;

import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import turtleMart.global.common.BaseEntity;

@Entity
@Getter
@Table(name = "tracking_number")
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class TrackingNumber extends BaseEntity {

    @Id
    private String trackingNumber;

    private TrackingNumber(String trackingNumber) {
        this.trackingNumber = trackingNumber;
    }

    public static TrackingNumber of(String trackingNumber) {
        return new TrackingNumber(trackingNumber);
    }
}
