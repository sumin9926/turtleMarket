package turtleMart.review.dto.response;

public record TemplateChoiceResponse(String question, String answer) {

    public static TemplateChoiceResponse of(String question, String answer){
        return new TemplateChoiceResponse(question, answer);
    }
}
