package turtleMart.member.service;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import turtleMart.member.dto.request.CardRegisterRequest;
import turtleMart.member.dto.response.CardResponse;
import turtleMart.member.entity.Card;
import turtleMart.member.repository.CardRepository;

@Service
@RequiredArgsConstructor
@Transactional
public class CardService {
    private final CardRepository cardRepository;

    public CardResponse cardRegister(CardRegisterRequest request) {
        Card card = Card.of(request);
        cardRepository.save(card);
        return CardResponse.from(card);
    }
}
