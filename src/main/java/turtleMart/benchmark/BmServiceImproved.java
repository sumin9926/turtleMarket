package turtleMart.benchmark;

import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@Profile({"benchmark","improved"})
@RequiredArgsConstructor
public class BmServiceImproved {

    private final BmDispatcher dispatcher;

    public void changePrice(long pocId, int newPrice) {
        dispatcher.changePrice(pocId, newPrice);
    }

    public void createOrder(List<Long> pocIdList, int quantity) {
        dispatcher.createOrder(pocIdList, quantity);
    }
}
