package expression.generic;

public abstract class UnaryOperation<T extends Number> {
    abstract T constant(String c);

    abstract T negate(String c);

    abstract T count(T x);

}
