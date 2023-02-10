package expression.generic;

public class CheckedMin<T extends Number> extends AbstractOperation<T> implements TripleExpression<T> {
    CheckedMin(TripleExpression<T> first, TripleExpression<T> second, NarrowType<T> type) {
        super(first, second, type);
    }

    @Override
    public T evaluate(T x, T y, T z) {
        return type.min(first.evaluate(x, y, z), second.evaluate(x, y, z));
    }
}
