package expression.generic;

import java.math.BigInteger;

public class BiExpression extends NarrowType<BigInteger> {

    @Override
    public BigInteger add(BigInteger x, BigInteger y) {
        return x.add(y);
    }

    @Override
    public BigInteger subtract(BigInteger x, BigInteger y) {
        return x.subtract(y);
    }

    @Override
    public BigInteger multiply(BigInteger x, BigInteger y) {
        return x.multiply(y);
    }

    @Override
    public BigInteger divide(BigInteger x, BigInteger y) {
        return x.divide(y);
    }

    @Override
    BigInteger min(BigInteger x, BigInteger y) {
        return x.min(y);
    }

    @Override
    BigInteger max(BigInteger x, BigInteger y) {
        return x.max(y);
    }
}
