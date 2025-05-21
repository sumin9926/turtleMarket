package turtleMart.payment.service;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import turtleMart.member.entity.Member;
import turtleMart.member.repository.MemberRepository;
import turtleMart.order.entity.Order;
import turtleMart.payment.dto.request.PaymentRequest;
import turtleMart.payment.dto.response.PaymentResponse;
import turtleMart.payment.entity.Payment;
import turtleMart.payment.repository.PaymentRepository;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class PaymentService {
    private final PaymentRepository paymentRepository;
    private final MemberRepository memberRepository;
    private final OrderRepository orderRepository;

    @Transactional
    public PaymentResponse createPayment(PaymentRequest request) {
        Member foundMember = memberRepository.findById(request.memberId()).orElseThrow(
                () -> new RuntimeException("")
        );
        Order foundOrder = orderRepository.findById(request.orderId()).orElseThrow(
                () -> new RuntimeException("")
        );

        Payment payment = Payment.of(
                foundOrder, foundMember,
                request.amount(),
                request.paymentMethod(),
                request.cardCompany(),
                request.installmentMonth());

        paymentRepository.save(payment);

        return PaymentResponse.from(payment);
    }
}
