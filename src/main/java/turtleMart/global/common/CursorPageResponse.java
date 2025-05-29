package turtleMart.global.common;

import turtleMart.review.entity.ReviewReport;

import java.util.List;

public record CursorPageResponse<T>(List<T> content, Long lastCursor) {

    public static<T> CursorPageResponse of(List<T> content, Long lastCursor){
        return new CursorPageResponse<T>(content, lastCursor);
    }

}
