package expression.generic;

public class CheckedConst<T extends Number> implements TripleExpression<T> {
    protected final String c;
    protected final UnaryOperation<T> type;
    CheckedConst(String c, UnaryOperation<T> type) {
        this.c = c;
        this.type = type;
    }

    @Override
    public T evaluate(T x, T y, T z) {
        return type.constant(c);
    }
}
