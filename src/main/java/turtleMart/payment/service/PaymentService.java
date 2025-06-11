package turtleMart.payment.service;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import turtleMart.global.exception.ErrorCode;
import turtleMart.global.exception.NotFoundException;
import turtleMart.member.entity.Member;
import turtleMart.member.repository.MemberRepository;
import turtleMart.order.entity.Order;
import turtleMart.order.repository.OrderRepository;
import turtleMart.payment.dto.request.PaymentDeductRequest;
import turtleMart.payment.dto.request.PaymentRequest;
import turtleMart.payment.dto.response.PaymentDeductResponse;
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
    public void createPayment(PaymentRequest request) {
        Member foundMember = memberRepository.findById(request.memberId()).orElseThrow(
                () -> new NotFoundException(ErrorCode.MEMBER_NOT_FOUND)
        );
        Order foundOrder = orderRepository.findById(request.orderId()).orElseThrow(
                () -> new NotFoundException(ErrorCode.ORDER_NOT_FOUND)
        );

        Payment payment = Payment.of(
                foundOrder, foundMember,
                request.amount(),
                request.paymentMethod(),
                request.cardCompany(),
                request.installmentMonth());

        paymentRepository.save(payment);

        PaymentResponse.from(payment);
    }

    public List<PaymentResponse> getMyAllPayments(Long memberId) {
        List<Payment> payments = paymentRepository.findAllByMemberId(memberId).orElseThrow(
                () -> new NotFoundException(ErrorCode.MEMBER_HAS_NO_PAYMENT)
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
                    () -> new NotFoundException(ErrorCode.MEMBER_HAS_NO_PAYMENT));
        } else {
            payments = paymentRepository.findAll();
        }

        return payments.stream().map(PaymentResponse::from).toList();
    }

    @Transactional
    public PaymentDeductResponse deductPayment(Long paymentId, Long memberId, PaymentDeductRequest request) {
        Payment foundPayment = findPayment(paymentId);
        checkOwnPayment(foundPayment, memberId);

        foundPayment.deduct(request.deductAmount());

        return PaymentDeductResponse.from(foundPayment);
    }

    private Payment findPayment(Long paymentId) {
        return paymentRepository.findById(paymentId).orElseThrow(
                () -> new NotFoundException(ErrorCode.PAYMENT_NOT_FOUND)
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
