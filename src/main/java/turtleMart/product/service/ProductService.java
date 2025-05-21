package turtleMart.product.service;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import turtleMart.member.entity.Seller;
import turtleMart.member.repository.SellerRepository;
import turtleMart.product.dto.ProductRequest;
import turtleMart.product.dto.ProductResponse;
import turtleMart.product.dto.ProductResponseForSeller;
import turtleMart.product.entity.Product;
import turtleMart.product.repository.ProductDslRepository;
import turtleMart.product.repository.ProductRepository;

import java.util.List;

@Service
@RequiredArgsConstructor
public class ProductService {

    private final SellerRepository sellerRepository;
    private final ProductRepository productRepository;
    private final ProductDslRepository productDslRepository;

    public ProductResponse createProduct(ProductRequest productRequest, Long sellerId) {
        Seller seller = sellerRepository.findById(sellerId).orElseThrow(() -> new RuntimeException(""));
        Product product = Product.of(seller, productRequest.name(), productRequest.price(), productRequest.description());
        productRepository.save(product);
        return ProductResponse.from(product);
    }

    public ProductResponse getProduct(Long productId) {
        Product product = productDslRepository.findByIdWithSeller(productId);
        return ProductResponse.from(product);
    }

    public List<ProductResponseForSeller> getProductBySellerId(Long sellerId) {
        List<Product> productList = productRepository.findAllBySellerId(sellerId);
        return productList.stream().map(ProductResponseForSeller::from).toList();
    }

    public ProductResponse updateProduct(ProductRequest productRequest, Long sellerId, Long productId) {
        Product product = productDslRepository.findByIdWithSeller(productId);
        extracted(sellerId, product);
        product.update(productRequest);
        productRepository.save(product);
        return ProductResponse.from(product);
    }

    public void deleteProduct(Long productId, Long sellerId) {
        Product product = productRepository.findById(productId).orElseThrow(() -> new RuntimeException(""));
        extracted(sellerId,product);
        productRepository.delete(product);
    }

    private void extracted(Long sellerId, Product product) {
        if (!product.getSeller().getId().equals(sellerId)) {
            throw new RuntimeException("");
        }
    }
}
