package turtleMart.delivery.entity;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import turtleMart.delivery.dto.reqeust.UpdateCourierRequest;
import turtleMart.global.common.BaseEntity;

@Entity
@Getter
@Table
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class Courier extends BaseEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private String name;

    private String code;

    private String trackingUrlTemplate;

    private boolean isDeleted = false;

    private Courier(String name, String code, String trackingUrlTemplate) {
        this.name = name;
        this.code = code;
        this.trackingUrlTemplate = trackingUrlTemplate;
    }

    public static Courier of(String name, String code, String trackingUrlTemplate) {
        return new Courier(name, code, trackingUrlTemplate);
    }

    public void update(UpdateCourierRequest request) {
        if (request.name() != null && !request.name().isBlank()) {
            this.name = request.name();
        }
        if (request.code() != null && !request.code().isBlank()) {
            this.code = request.code();
        }
        if (request.trackingUrlTemplate() != null && !request.trackingUrlTemplate().isBlank()) {
            this.trackingUrlTemplate = request.trackingUrlTemplate();
        }
    }

    public void delete() {
        this.isDeleted = true;
    }
}
