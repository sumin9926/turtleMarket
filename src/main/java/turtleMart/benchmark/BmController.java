package turtleMart.benchmark;

import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Profile;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
@Profile("benchmark")
@RequestMapping("/bm")
@RequiredArgsConstructor
public class BmController {

    private final BmServiceBaseline serviceBaseline;
    private final BmServiceImproved serviceImproved;

    // Baseline Code
    @PostMapping("/baseline/price-change")  // 가격변경
    public ResponseEntity<Void> changePriceV1(
            @RequestParam long pocId,
            @RequestParam int newPrice
    ) {
        serviceBaseline.changePrice(pocId, newPrice);
        return ResponseEntity.status(HttpStatus.OK).build();
    }

    @PostMapping("/baseline/order-create")  // 주문생성
    public ResponseEntity<Void> orderV1(
            @RequestParam List<Long> pocIdList,
            @RequestParam int quantity
    ) {
        serviceBaseline.createOrder(pocIdList, quantity);
        return ResponseEntity.status(HttpStatus.OK).build();
    }

    // Improved Code
    @PostMapping("/improved/price-change")  // 가격변경
    public ResponseEntity<Void> changePriceV2(
            @RequestParam long pocId,
            @RequestParam int newPrice
    ) {
        serviceImproved.changePrice(pocId, newPrice);
        return ResponseEntity.status(HttpStatus.OK).build();
    }

    @PostMapping("/improved/order-create")  // 주문생성
    public ResponseEntity<Void> orderV2(
            @RequestParam List<Long> pocIdList,
            @RequestParam int quantity
    ) {
        serviceImproved.createOrder(pocIdList, quantity);
        return ResponseEntity.status(HttpStatus.OK).build();
    }

}
