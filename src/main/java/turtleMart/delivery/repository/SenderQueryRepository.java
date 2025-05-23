package turtleMart.delivery.repository;

import turtleMart.delivery.entity.Courier;

public interface SenderQueryRepository {

    long countByCourier(Courier courier);
}
