package expression.generic;

public class CheckedCount<T extends Number> extends AbstractUnary<T> implements TripleExpression<T>  {
    CheckedCount(TripleExpression<T> expr, UnaryOperation<T> type) {
        super(expr, type);
    }

    @Override
    public T evaluate(T x, T y, T z) {
        return type.count(expr.evaluate(x, y, z));
    }
}
