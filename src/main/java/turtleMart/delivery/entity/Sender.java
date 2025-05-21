package turtleMart.delivery.entity;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;
import turtleMart.delivery.dto.reqeust.UpdateSenderRequest;

import java.time.LocalDateTime;

@Entity
@Getter
@Table
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@EntityListeners(AuditingEntityListener.class)
public class Sender {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "courier_id", nullable = false)
    private Courier courier;

    @Column(unique = true, nullable = false)
    private String name;

    @Column(name = "phone_number", unique = true, nullable = false)
    private String phoneNumber;

    private String address;

    private String detailAddress;

    private boolean isDeleted = false;

    @CreatedDate
    private LocalDateTime createdAt;

    @LastModifiedDate
    private LocalDateTime updatedAt;

    private Sender(Courier courier, String name, String phoneNumber, String address, String detailAddress) {
        this.courier = courier;
        this.name = name;
        this.phoneNumber = phoneNumber;
        this.address = address;
        this.detailAddress = detailAddress;
    }

    public static Sender of (Courier courier, String name, String phoneNumber, String address, String detailAddress) {
        return new Sender(courier, name, phoneNumber, address, detailAddress);
    }

    public void update(UpdateSenderRequest request) {
        if (request.name() != null && !request.name().isBlank()) {
            this.name = request.name();
        }
        if (request.phoneNumber() != null && !request.phoneNumber().isBlank()) {
            this.phoneNumber = request.phoneNumber();
        }
        if (request.address() != null && !request.address().isBlank()) {
            this.address = request.address();
        }
        if (request.detailAddress() != null && !request.detailAddress().isBlank()) {
            this.detailAddress = request.detailAddress();
        }
    }

    public void delete() {
        this.isDeleted = true;
    }
}
