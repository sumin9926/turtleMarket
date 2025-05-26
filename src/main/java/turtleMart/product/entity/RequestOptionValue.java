package turtleMart.product.entity;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Entity
@Getter
@Table
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class RequestOptionValue {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private String name;

    @Enumerated(EnumType.STRING)
    private RequestOptionValueStatus requestOptionValueStatus;

    private RequestOptionValue(String name, RequestOptionValueStatus requestOptionValueStatus) {
        this.name = name;
        this.requestOptionValueStatus = requestOptionValueStatus;
    }

    public static RequestOptionValue of(String name) {
        return new RequestOptionValue(name, RequestOptionValueStatus.PENDING);
    }

    public void updateStatus(RequestOptionValueStatus requestOptionValueStatus) {
        this.requestOptionValueStatus = requestOptionValueStatus;
    }
}
