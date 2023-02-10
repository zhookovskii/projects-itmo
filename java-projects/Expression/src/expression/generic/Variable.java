package expression.generic;

public class Variable<T extends Number> implements TripleExpression<T> {
    protected final String c;
    protected final UnaryOperation<T> type;

    Variable(String c, UnaryOperation<T> type) {
        this.c = c;
        this.type = type;
    }

    @Override
    public T evaluate(T x, T y, T z) {
        switch (c) {
            case "x":
                return type.constant(x.toString());
            case "y":
                return type.constant(y.toString());
            case "z":
                return type.constant(z.toString());
            default:
                throw new AssertionError("Unexpected variable: " + c);
        }
    }
}
