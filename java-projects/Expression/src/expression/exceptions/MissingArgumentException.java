package expression.exceptions;

public class MissingArgumentException extends ParsingException {
    String message;

    public MissingArgumentException() {
        super();
    }

    public MissingArgumentException(String msg) {
        this.message = msg;
    }

    @Override
    public String getMessage() {
        return message;
    }

}
