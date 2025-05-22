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

import java.util.List;

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

    public List<PaymentResponse> getMyAllPayments(Long memberId) {
        List<Payment> payments = paymentRepository.findAllByMemberId(memberId).orElseThrow(
                () -> new RuntimeException("")
        );

        return payments.stream().map(PaymentResponse::from).toList();
    }

    public PaymentResponse getMyPayment(Long paymentId, Long memberId) {
        Payment foundPayment = findPayment(paymentId);
        checkOwnPayment(foundPayment, memberId);
        return PaymentResponse.from(foundPayment);
    }

    public List<PaymentResponse> getAllPayments(Long memberId) {
        checkAdmin(memberId);
        List<Payment> payments;
        if (memberId != null) {
            payments = paymentRepository.findAllByMemberId(memberId).orElseThrow(
                    () -> new RuntimeException("")
            );
        } else {
            payments = paymentRepository.findAll();
        }

        return payments.stream().map(PaymentResponse::from).toList();
    }



    private Payment findPayment(Long paymentId) {
        return paymentRepository.findById(paymentId).orElseThrow(
                () -> new RuntimeException("")
        );
    }

    private void checkOwnPayment(Payment payment, Long memberId) {
        if (!payment.getMember().getId().equals(memberId)) {
            throw new RuntimeException("");
        }
    }

    private void checkAdmin(Long loggedInId) {
//        if (!foundMember.getUserRole().equals(UserRole.ADMIN)) {
//            throw new RuntimeException("");
//        }
    }
}
