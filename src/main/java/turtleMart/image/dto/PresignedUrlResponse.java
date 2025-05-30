package turtleMart.image.dto;

public record PresignedUrlResponse(String url) {
    public static PresignedUrlResponse of(String url){
        return new PresignedUrlResponse(url);
    }
}
