package turtleMart.benchmark.dto;

public record MakeRequest(
        String type, // type: "PRICE_CHANGE", "ORDER_CREATE"
        long pocId,
        int quantity,
        int newPrice,
        int attempt
) { }

