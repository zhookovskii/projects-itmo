package expression.exceptions;

public class UnexpectedArgumentException extends ParsingException {
    String message;

    public UnexpectedArgumentException() {
        super();
    }

    public UnexpectedArgumentException(String msg) {
        this.message = msg;
    }

    @Override
    public String getMessage() {
        return message;
    }

}
