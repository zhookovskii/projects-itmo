package expression.generic;

import java.math.BigInteger;

public interface TripleExpression<T extends Number> {
    T evaluate(T x, T y, T z);
}
