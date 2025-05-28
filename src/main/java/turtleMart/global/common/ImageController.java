package turtleMart.global.common;

import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestHeader;

import java.util.Map;

@Controller
@RequiredArgsConstructor
public class ImageController {

    private final ImageService imageService;

    @GetMapping("/images")
    public String imageUrl() {
        return "image/upload";
    }

    @GetMapping("/images/upload/{imageName}")
    public ResponseEntity<Map<String, String>> loadImageFile(
            @PathVariable(name = "imageName") String imageName,
            @RequestHeader(value = "Referer", required = false) String referer) {

        String domainPrefix;
        if(referer.contains("product")){domainPrefix = "product";}
        else{domainPrefix = "review";}

        return ResponseEntity.ok(Map.of("data", imageService.uploadFile(domainPrefix, imageName)));
    }


    @GetMapping("/images/view/{imageName}")
    public ResponseEntity<Map<String, String>> loadFile(@PathVariable(name = "imageName") String imageName) {
        return ResponseEntity.ok(Map.of("data", imageService.loadFile(imageName)));
    }
}
