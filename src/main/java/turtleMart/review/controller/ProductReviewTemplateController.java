package turtleMart.review.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.RestController;
import turtleMart.review.service.ProductReviewTemplateService;

@RestController
@RequiredArgsConstructor
public class ProductReviewTemplateController {

    private final ProductReviewTemplateService productReviewTemplateService;
}
