package turtleMart.order.service;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import turtleMart.order.dto.request.CartOrderSheetRequest;
import turtleMart.order.dto.response.OrderSheetResponse;
import turtleMart.product.entity.Product;
import turtleMart.product.repository.ProductRepository;

import java.util.ArrayList;
import java.util.List;

@Service
@RequiredArgsConstructor
public class OrderService {

    private final ProductRepository productRepository;

    @Transactional(readOnly = true)
    public List<OrderSheetResponse> getCartOrderSheet(List<CartOrderSheetRequest> orderSheetList) {

        /*TODO 가격/품절상태 검증 필요*/

        List<OrderSheetResponse> responseList = new ArrayList<>();

        for(CartOrderSheetRequest orderSheet : orderSheetList){
            Product product = productRepository.findById(orderSheet.productId()).orElseThrow(
                    ()->new RuntimeException("상품이 존재하지 않습니다.") //TODO 커스텀 예외처리
            );
            OrderSheetResponse response = new OrderSheetResponse(
                    product.getId(), product.getName(), product.getPrice(), orderSheet.quantity()
            );
            responseList.add(response);
        }

        return responseList;
    }
}
