package turtleMart.member.controller;

import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import turtleMart.member.dto.request.CardRegisterRequest;
import turtleMart.member.dto.response.CardResponse;
import turtleMart.member.service.CardService;

@RestController
@RequestMapping("/members/userProfile/cards")
@RequiredArgsConstructor
public class CardController {
    private final CardService cardService;

    /**
     * 카드 등록
     */
    @PostMapping("register")
    public ResponseEntity<CardResponse> cardRegister(
            @RequestBody @Valid CardRegisterRequest request
    ) {
        CardResponse cardResponse = cardService.cardRegister(request);
        return ResponseEntity.status(HttpStatus.CREATED).body(cardResponse);
    }

    /**
     * 카드 조회
     */

    /**
     * 카드 삭제
     */
}
