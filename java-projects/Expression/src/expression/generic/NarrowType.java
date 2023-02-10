package expression.generic;

public abstract class NarrowType<T extends Number> {
    abstract T add(T x, T y);

    abstract T subtract(T x, T y);

    abstract T multiply(T x, T y);

    abstract T divide(T x, T y);

    abstract T min(T x, T y);

    abstract T max(T x, T y);
}
