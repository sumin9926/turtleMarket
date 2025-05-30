package turtleMart.review.service;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import turtleMart.global.exception.BadRequestException;
import turtleMart.global.exception.ErrorCode;
import turtleMart.global.exception.NotFoundException;
import turtleMart.global.exception.RoleMismatchException;
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

    @Transactional
    public void createProductReviewTemplate(
             Long productId, CreateProductReviewTemplateRequest request){

        if(!sellerRepository.existsById(request.sellerId())){throw new NotFoundException(ErrorCode.SELLER_NOT_FOUND);}
        if(!productRepository.existsById(productId)){throw new NotFoundException(ErrorCode.PRODUCT_NOT_FOUND);}

        Seller seller = sellerRepository.getReferenceById(request.sellerId());
        Product product = productRepository.getReferenceById(productId);

        Long productSellerId = product.getSeller().getId();
        if(!seller.getId().equals(productSellerId)){throw new RoleMismatchException(ErrorCode.FORBIDDEN);}


        List<ReviewTemplate> reviewTemplateList = reviewTemplateRepository.findAllById(request.reviewTemplateIdList());
        if(request.reviewTemplateIdList().size() != reviewTemplateList.size()){
            throw new NotFoundException(ErrorCode.REVIEW_TEMPLATE_NOT_FOUND);
        }

        List<Long> reviewTemplateIdList =  reviewTemplateList.stream().mapToLong(ReviewTemplate::getId).boxed().toList();
        if(!productReviewTemplateDslRepository.existsByProductIdAndReviewTemplateId(product.getId(),reviewTemplateIdList)){
            throw new BadRequestException(ErrorCode.DUPLICATE_TEMPLATE_SELECTION); // 같은 리뷰 템플릿 두번 선택하려고 할 경우
        }

        List<ProductReviewTemplate> productReviewTemplateList = new ArrayList<>();
        reviewTemplateList.forEach(r -> productReviewTemplateList.add(ProductReviewTemplate.of(product, r)));

        productReviewTemplateRepository.saveAll(productReviewTemplateList);
    }

    @Transactional
    public void deleteProductReviewTemplate(Long memberId, Long productReviewTemplateId){
        Seller seller = sellerRepository.findByMemberId(memberId)
               .orElseThrow(() -> new NotFoundException(ErrorCode.SELLER_NOT_FOUND));


       ProductReviewTemplate productReviewTemplate = productReviewTemplateRepository.findById(productReviewTemplateId)
                       .orElseThrow(() -> new NotFoundException(ErrorCode.PRODUCT_REVIEW_TEMPLATE_NOT_FOUND));

       Long productSellerId = productReviewTemplate.getProduct().getSeller().getId();
       if(!seller.getId().equals(productSellerId)){
           throw new RoleMismatchException(ErrorCode.FORBIDDEN);
       }
        productReviewTemplateRepository.deleteById(productReviewTemplateId);
    }
}
