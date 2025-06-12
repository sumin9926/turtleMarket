package turtleMart.member.service;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import turtleMart.member.dto.request.AccountRegisterRequest;
import turtleMart.member.dto.response.AccountResponse;
import turtleMart.member.entity.BankAccount;
import turtleMart.member.repository.AccountRepository;

@Service
@RequiredArgsConstructor
public class AccountService {
    private final AccountRepository accountRepository;

    public AccountResponse accountRegister(AccountRegisterRequest request) {
        BankAccount bankAccount = BankAccount.of(request);
        accountRepository.save(bankAccount);
        return AccountResponse.from(bankAccount);
    }
}
