package turtleMart.product.entity;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import turtleMart.global.exception.BadRequestException;
import turtleMart.global.exception.ErrorCode;
import turtleMart.member.entity.Seller;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

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

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "seller_id")
    private Seller seller;

    @OneToMany(mappedBy = "requestOptionGroup", cascade = CascadeType.ALL, orphanRemoval = true)
    private List<RequestOptionValue> requestOptionValueList = new ArrayList<>();

    private RequestOptionGroup(String name, Seller seller) {
        this.name = name;
        this.seller = seller;
    }

    public static RequestOptionGroup of(String name,Seller seller) {
        return new RequestOptionGroup(name, seller);
    }

    public void addValue(RequestOptionValue requestOptionValue) {
        requestOptionValue.addGroup(this);
        requestOptionValueList.add(requestOptionValue);
    }

    public List<RequestOptionValue> extractSelectValue(List<Long> selectIdList) {
        Map<Long, RequestOptionValue> valueMap = this.requestOptionValueList.stream()
                .collect(Collectors.toMap(RequestOptionValue::getId, Function.identity()));

        return selectIdList.stream()
                .map(id -> {
                    RequestOptionValue value = valueMap.get(id);
                    if (value == null) {
                        throw new BadRequestException(
                                ErrorCode.REQUEST_OPTION_VALUE_NOT_FOUND,
                                "해당 옵션 값 ID는 요청 그룹에 존재하지 않습니다. ID = " + id
                        );
                    }
                    return value;
                })
                .toList();
    }
}
