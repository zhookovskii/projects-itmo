package expression.handmadeExceptions;

public class ConstOverflowException extends ParsingException {
    String message;

    public ConstOverflowException() {
        super();
    }

    public ConstOverflowException(String msg) {
        this.message = msg;
    }

    @Override
    public String getMessage() {
        return message;
    }
}
