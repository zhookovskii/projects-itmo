package expression.handmadeExceptions;

public class BracketSequenceException extends ParsingException {
    String message;
    public BracketSequenceException() {
        super();
    }

    public BracketSequenceException(String msg) {
        this.message = msg;
    }

    @Override
    public String getMessage() {
        return message;
    }

}
