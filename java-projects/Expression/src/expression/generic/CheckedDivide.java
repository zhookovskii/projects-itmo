package expression.generic;

public class CheckedDivide<T extends Number> extends AbstractOperation<T> implements TripleExpression<T> {

    public CheckedDivide(TripleExpression<T> first, TripleExpression<T> second, NarrowType<T> type) {
        super(first, second, type);
    }

    @Override
    public T evaluate(T x, T y, T z) {
        return type.divide(first.evaluate(x, y, z), second.evaluate(x, y, z));
    }

    @Override
    public String toString() {
        return toString("/");
    }
}
