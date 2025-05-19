package turtleMart.delivery.entity;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Entity
@Getter
@Table
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class TrackingNumber {

    @Id
    private Long id;

    @ManyToOne
    @JoinColumn
    private Delivery delivery;

    private LocalDateTime shippedAt;

}
