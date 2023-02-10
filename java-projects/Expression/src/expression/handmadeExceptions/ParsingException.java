package expression.handmadeExceptions;

public class ParsingException extends Exception {

    public ParsingException() {
        super();
    }

    public String getMessage() {
        return "Parsing error";
    }
}
