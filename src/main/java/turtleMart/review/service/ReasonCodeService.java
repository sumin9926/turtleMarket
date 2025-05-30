package turtleMart.review.service;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import turtleMart.global.exception.BadRequestException;
import turtleMart.global.exception.ErrorCode;
import turtleMart.global.exception.NotFoundException;
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
                .orElseThrow(() -> new NotFoundException(ErrorCode.REASON_CODE_NOT_FOUND));

        if(reasonCode.isDeleted()){throw new BadRequestException(ErrorCode.ALREADY_DELETED_REASON_CODE);}

        reasonCode.update(request.reason());
        return ReasonCodeResponse.from(reasonCode);
    }

    @Transactional
    public void delete(Long reasonCodeId){
        ReasonCode reasonCode = reasonCodeRepository.findById(reasonCodeId)
                .orElseThrow(() -> new NotFoundException(ErrorCode.REASON_CODE_NOT_FOUND));

        if(reasonCode.isDeleted()){throw new BadRequestException(ErrorCode.ALREADY_DELETED_REASON_CODE);}

        reasonCode.delete();
    }
}
