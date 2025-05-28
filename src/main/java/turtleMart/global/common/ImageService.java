package turtleMart.global.common;


import io.minio.*;
import io.minio.http.Method;
import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.UUID;

@Component
@RequiredArgsConstructor
public class ImageService {

    private final MinioClient minioClient;
    final String BUCKET = "images";


    public String uploadFile(String domainPrefix, String objectName) {

        LocalDate today = LocalDate.now();
        String prefix = domainPrefix + today.format(DateTimeFormatter.ofPattern("/yyyy/MM/dd/"));

        GetPresignedObjectUrlArgs preUrlRequest = GetPresignedObjectUrlArgs.builder()
                .method(Method.PUT)
                .bucket(BUCKET)
                .object(prefix + (UUID.randomUUID().getMostSignificantBits() & Long.MAX_VALUE) + objectName)// 혹시 모를 이미지 이름이 같을 경우를 대비해 uuid사용
                .expiry(300)
                .build();

        try{
            return minioClient.getPresignedObjectUrl(preUrlRequest);
        }catch (Exception e){
            throw new RuntimeException("이미지 업로드 요청중 문제가 발생했습니다");
        }
    }

    public String loadFile(String fileName){

        GetPresignedObjectUrlArgs getUrlRequest = GetPresignedObjectUrlArgs.builder()
                .method(Method.GET)
                .bucket(BUCKET)
                .object(fileName)
                .expiry(600)
                .build();

        try{
           return minioClient.getPresignedObjectUrl(getUrlRequest);
        }catch (Exception e){
            throw new RuntimeException("이미지 로드 중 문제가 발생했습니다");
        }
    }


    @PostConstruct
    private void isExistBucket() {
        try {
            if (!minioClient.bucketExists(BucketExistsArgs.builder().bucket(BUCKET).build())) {
                minioClient.makeBucket(MakeBucketArgs.builder().bucket(BUCKET).build());
            }
        } catch (Exception ex) {
            throw new RuntimeException("버킷 초기설정중 예외발생");
        }
    }
}
