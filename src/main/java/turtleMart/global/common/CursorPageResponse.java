package turtleMart.global.common;

import turtleMart.review.entity.ReviewReport;

import java.util.List;

public record CursorPageResponse<T>(List<T> content, Long lastCursor, boolean isLastPage) {

    public static<T> CursorPageResponse of(List<T> content, Long lastCursor, boolean isLastPage){
        return new CursorPageResponse<T>(content, lastCursor,isLastPage);
    }

}
