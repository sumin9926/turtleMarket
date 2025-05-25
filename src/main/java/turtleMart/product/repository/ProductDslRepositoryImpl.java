package turtleMart.product.repository;

import com.querydsl.jpa.impl.JPAQueryFactory;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;
import turtleMart.member.entity.QSeller;
import turtleMart.product.entity.Product;
import turtleMart.product.entity.QProduct;

import java.util.List;

@Repository
@RequiredArgsConstructor
public class ProductDslRepositoryImpl implements ProductDslRepository {

    private final JPAQueryFactory jpaQueryFactory;

    @Override
    public Product findByIdWithSeller(Long productId) {
        QProduct product = QProduct.product;
        QSeller seller = QSeller.seller;
        return jpaQueryFactory.selectFrom(product)
                .join(product.seller, seller).fetchJoin()
                .where(product.id.eq(productId))
                .fetchOne();
    }
}
