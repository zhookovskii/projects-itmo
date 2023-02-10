package expression.handmadeExceptions;

public class WrongOperatorException extends ParsingException {
    String message;

    public WrongOperatorException() {
        super();
    }

    public WrongOperatorException(String msg) {
        this.message = msg;
    }

    @Override
    public String getMessage() {
        return message;
    }
}
