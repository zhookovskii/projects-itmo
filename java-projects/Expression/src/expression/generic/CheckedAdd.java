package expression.generic;

public class CheckedAdd<T extends Number> extends AbstractOperation<T> implements TripleExpression<T> {

    public CheckedAdd(TripleExpression<T> first, TripleExpression<T> second, NarrowType<T> type) {
        super(first, second, type);
    }

    @Override
    public T evaluate(T x, T y, T z) {
        return type.add(first.evaluate(x, y, z), second.evaluate(x, y, z));
    }

    @Override
    public String toString() {
        return toString("+");
    }

}
