package expression.generic;

public class AbstractUnary<T extends Number> {
    protected final TripleExpression<T> expr;

    protected UnaryOperation<T> type;

    AbstractUnary(TripleExpression<T> expr, UnaryOperation<T> type) {
        this.expr = expr;
        this.type = type;
    }

}
