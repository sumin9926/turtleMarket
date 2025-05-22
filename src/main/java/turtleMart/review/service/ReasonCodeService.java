package turtleMart.review.service;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import turtleMart.review.dto.request.CreateReasonCodeRequest;
import turtleMart.review.dto.request.UpdateReasonCodeRequest;
import turtleMart.review.dto.response.ReasonCodeResponse;
import turtleMart.review.entity.ReasonCode;
import turtleMart.review.repository.ReasonCodeRepository;

import java.util.List;

@Service
@Transactional(readOnly = true)
@RequiredArgsConstructor
public class ReasonCodeService {

    private final ReasonCodeRepository reasonCodeRepository;

    @Transactional
    public ReasonCodeResponse createReasonCode(CreateReasonCodeRequest request){
        ReasonCode reasonCode = reasonCodeRepository.save(ReasonCode.of(request.reason()));
        return ReasonCodeResponse.from(reasonCode);
    }

    public List<ReasonCodeResponse> readAll(){
        return reasonCodeRepository.findAllIsDeletedFalse().stream().map(ReasonCodeResponse::from).toList();
    }

    @Transactional
    public ReasonCodeResponse updateReasonCode(Long reasonCodeId, UpdateReasonCodeRequest request){
        ReasonCode reasonCode = reasonCodeRepository.findById(reasonCodeId)
                .orElseThrow(() -> new RuntimeException("존재하지 않는 신고코드입니다"));

        if(reasonCode.isDeleted()){throw new RuntimeException("삭제된 신고코드입니다");}

        reasonCode.update(request.reason());
        return ReasonCodeResponse.from(reasonCode);
    }

    @Transactional
    public void delete(Long reasonCodeId){
        ReasonCode reasonCode = reasonCodeRepository.findById(reasonCodeId)
                .orElseThrow(() -> new RuntimeException("존재하지 않는 신고이유입니다"));

        if(reasonCode.isDeleted()){throw new RuntimeException("삭제된 신고코드입니다");}

        reasonCode.delete();
    }
}
