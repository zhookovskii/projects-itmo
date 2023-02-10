package expression.exceptions;

public class MissingOperationException extends ParsingException {
    String message;

    public MissingOperationException() {
        super();
    }

    public MissingOperationException(String msg) {
        this.message = msg;
    }

    @Override
    public String getMessage() {
        return message;
    }
}
