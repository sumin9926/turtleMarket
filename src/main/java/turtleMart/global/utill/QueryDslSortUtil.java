package turtleMart.global.utill;

import com.querydsl.core.types.Expression;
import com.querydsl.core.types.Order;
import com.querydsl.core.types.OrderSpecifier;
import com.querydsl.core.types.dsl.ComparableExpressionBase;
import com.querydsl.core.types.dsl.EntityPathBase;
import com.querydsl.core.types.dsl.PathBuilder;
import org.springframework.data.domain.Sort;
import turtleMart.product.entity.QProductOptionGroup;

public class QueryDslSortUtil {
    public static OrderSpecifier<?>[] toOrderSpecifiers(Sort sort, EntityPathBase<?> root) {
        return sort.stream()
                .map(order -> {
                    PathBuilder<? extends Comparable> pathBuilder = new PathBuilder<>(Comparable.class, root.getMetadata());
                    ComparableExpressionBase<?> expression = pathBuilder.getComparable(order.getProperty(), Comparable.class);

                    return order.isAscending()
                            ? new OrderSpecifier<>(Order.ASC, expression)
                            : new OrderSpecifier<>(Order.DESC, expression);
                })
                .toArray(OrderSpecifier[]::new);
    }
}
