package turtleMart.member.controller;

import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import turtleMart.member.dto.request.AccountRegisterRequest;
import turtleMart.member.dto.response.AccountResponse;
import turtleMart.member.service.AccountService;

@RestController
@RequestMapping("/members/userProfile/accounts")
@RequiredArgsConstructor
public class AccountController {
    private final AccountService accountService;

    /**
     * 계좌 등록
     */
    @PostMapping("/register")
    public ResponseEntity<AccountResponse> accountRegister(
            @RequestBody @Valid AccountRegisterRequest request
    ) {
        AccountResponse accountResponse = accountService.accountRegister(request);
        return ResponseEntity.status(HttpStatus.CREATED).body(accountResponse);
    }

    /**
     * 계좌 조회
     */

    /**
     * 계좌 삭제
     */
}
