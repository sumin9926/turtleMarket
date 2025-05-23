package turtleMart.delivery.repository;

import turtleMart.delivery.entity.Delivery;

import java.util.List;

public interface DeliveryQueryRepository {

    List<Delivery> findAllByMemberId(Long memberId);

    List<Delivery> findAllBySeller(Long sellerId);
}
