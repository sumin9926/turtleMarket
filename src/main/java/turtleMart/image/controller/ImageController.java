package turtleMart.image.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import turtleMart.image.service.ImageService;
import turtleMart.image.dto.PresignedUrlResponse;

@Controller
@RequiredArgsConstructor
public class ImageController {

    private final ImageService imageService;

    @GetMapping("/images")
    public String imageUrl() {
        return "image/upload";
    }

    @GetMapping("/images/upload/{domainPrefix}/{imageName}")
    public ResponseEntity<PresignedUrlResponse> getPresignedUploadUrl(@PathVariable(name = "imageName") String imageName,
                                                                      @PathVariable(name = "domainPrefix") String domainPrefix) {
        return ResponseEntity.ok(imageService.generatePresignedUploadUrl(domainPrefix, imageName));
    }


    @GetMapping("/images/view/{imageName}")
    public ResponseEntity<PresignedUrlResponse> getPresignedViewUrl(@PathVariable(name = "imageName") String imageName) {
        return ResponseEntity.ok( imageService.generatePresignedAccessUrl(imageName));
    }
}
