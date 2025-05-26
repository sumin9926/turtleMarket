package turtleMart.product.entity;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

@Entity
@Getter
@Table
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class RequestOptionGroup {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private String name;

    private String rejectionReason;

    @OneToMany(mappedBy = "requestOptionGroup", cascade = CascadeType.ALL, orphanRemoval = true)
    private List<RequestOptionValue> requestOptionValueList = new ArrayList<>();

    private RequestOptionGroup(String name) {
        this.name = name;
    }

    public static RequestOptionGroup of(String name) {
        return new RequestOptionGroup(name);
    }
}
