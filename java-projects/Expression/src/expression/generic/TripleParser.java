package expression.generic;

@FunctionalInterface
public interface TripleParser<T extends Number> {
    TripleExpression<T> parse(String expression, NarrowType<T> type, UnaryOperation<T> unary) throws Exception;
}
