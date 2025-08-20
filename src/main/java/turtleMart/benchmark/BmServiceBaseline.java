package turtleMart.benchmark;

import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import turtleMart.global.exception.ErrorCode;
import turtleMart.global.exception.NotFoundException;
import turtleMart.product.entity.ProductOptionCombination;
import turtleMart.product.repository.ProductOptionCombinationRepository;

import java.util.List;

@Service
@Profile({"benchmark","baseline"})
@RequiredArgsConstructor
public class BmServiceBaseline {
    private final ProductOptionCombinationRepository optionCombinationRepository;

    @Transactional
    public void changePrice(long pocId, int newPrice) {
        if (newPrice < 0) throw new IllegalArgumentException("price < 0");

        ProductOptionCombination poc = optionCombinationRepository.findByIdWithPessimisticLock(pocId).orElseThrow(
                () -> new NotFoundException(ErrorCode.PRODUCT_OPTION_COMBINATION_NOT_FOUND)
        );

        poc.updatePrice(newPrice);
    }

    @Transactional
    public void createOrder(List<Long> pocIdList, int quantity) {
        if (quantity <= 0) throw new IllegalArgumentException("quantity <= 0");
        if (pocIdList == null || pocIdList.isEmpty()) {
            throw new IllegalArgumentException("pocIdList is empty");
        }

        List<ProductOptionCombination> pocList = optionCombinationRepository.findAllByIdIn(pocIdList);

        if(pocList.isEmpty()) throw new NotFoundException(ErrorCode.PRODUCT_OPTION_COMBINATION_NOT_FOUND);

        for(ProductOptionCombination poc:pocList){
            poc.decreaseInventory(quantity);
        }
    }
}
