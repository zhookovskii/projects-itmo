package expression.generic;

public class CheckedNegate<T extends Number> extends AbstractUnary<T> implements TripleExpression<T> {

    CheckedNegate(TripleExpression<T> expr, UnaryOperation<T> type) {
        super(expr, type);
    }

    @Override
    public T evaluate(T x, T y, T z) {
        return type.negate(expr.evaluate(x, y, z).toString());
    }
}
