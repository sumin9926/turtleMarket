package turtleMart.review.service;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import turtleMart.member.entity.Seller;
import turtleMart.member.repository.SellerRepository;
import turtleMart.product.entity.Product;
import turtleMart.product.repository.ProductRepository;
import turtleMart.review.dto.request.CreateProductReviewTemplateRequest;
import turtleMart.review.entity.ProductReviewTemplate;
import turtleMart.review.entity.ReviewTemplate;
import turtleMart.review.repository.ProductReviewTemplateDslRepositoryImpl;
import turtleMart.review.repository.ProductReviewTemplateRepository;
import turtleMart.review.repository.ReviewTemplateRepository;
import java.util.ArrayList;
import java.util.List;

@Service
@RequiredArgsConstructor
public class ProductReviewTemplateService {

    private final ProductReviewTemplateRepository productReviewTemplateRepository;
    private final SellerRepository sellerRepository;
    private final ProductRepository productRepository;
    private final ReviewTemplateRepository reviewTemplateRepository;
    private final ProductReviewTemplateDslRepositoryImpl productReviewTemplateDslRepository;

    public void createProductReviewTemplate(
             Long productId, CreateProductReviewTemplateRequest request){

        if(!sellerRepository.existsById(request.sellerId())){throw new RuntimeException("존재하지 않는 판매자입니다");}
        if(!productRepository.existsById(productId)){throw new RuntimeException("존재하지 않는 상품입니다");}

        Seller seller = sellerRepository.getReferenceById(request.sellerId());
        Product product = productRepository.getReferenceById(productId);

        Long productSellerId = product.getSeller().getId();
        if(!seller.getId().equals(productSellerId)){throw new RuntimeException("해당 상품에 대한 권한이 없습니다");}


        List<ReviewTemplate> reviewTemplateList = reviewTemplateRepository.findAllById(request.reviewTemplateIdList());
        if(request.reviewTemplateIdList().size() != reviewTemplateList.size()){
            throw new RuntimeException("존재하지 않는 리뷰템플릿을 선택하셨습니다");
        }

        List<Long> reviewTemplateIdList =  reviewTemplateList.stream().mapToLong(ReviewTemplate::getId).boxed().toList();
        if(!productReviewTemplateDslRepository.existsByProductIdAndReviewTemplateId(product.getId(),reviewTemplateIdList)){
            throw new RuntimeException("상품은 같은 리뷰템플릿을 여러개 선택할 수 없습니다");
        }

        List<ProductReviewTemplate> productReviewTemplateList = new ArrayList<>();
        reviewTemplateList.forEach(r -> productReviewTemplateList.add(ProductReviewTemplate.of(product, r)));

        productReviewTemplateRepository.saveAll(productReviewTemplateList);
    }

    public void deleteProductReviewTemplate(Long memberId, Long productReviewTemplateId){

        Seller seller = sellerRepository.findByMemberId(memberId)
               .orElseThrow(() -> new RuntimeException("존재하지 않는 판매자입니다"));


       ProductReviewTemplate productReviewTemplate = productReviewTemplateRepository.findById(productReviewTemplateId)
                       .orElseThrow(() -> new RuntimeException("존재하지 않는 상품리뷰 템플릿입니다"));

       Long productSellerId = productReviewTemplate.getProduct().getSeller().getId();
       if(!seller.getId().equals(productSellerId)){
           throw new RuntimeException("해당 상품에 대한 권한이 없습니다");
       }
        productReviewTemplateRepository.deleteById(productReviewTemplateId);
    }
}
