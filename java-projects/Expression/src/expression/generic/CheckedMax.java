package expression.generic;

public class CheckedMax<T extends Number> extends AbstractOperation<T> implements TripleExpression<T> {
    CheckedMax(TripleExpression<T> first, TripleExpression<T> second, NarrowType<T> type) {
        super(first, second, type);
    }

    @Override
    public T evaluate(T x, T y, T z) {
        return type.max(first.evaluate(x, y, z), second.evaluate(x, y, z));
    }
}
