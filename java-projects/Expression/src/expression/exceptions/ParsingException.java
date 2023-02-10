package expression.exceptions;

public class ParsingException extends Exception {

    public ParsingException() {
        super();
    }

    public String getMessage() {
        return "Parsing error";
    }
}
