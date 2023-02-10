package expression.generic;

import expression.common.Node;

import java.math.BigInteger;

public class BiUnary extends UnaryOperation<BigInteger> {

    @Override
    public BigInteger constant(String c) {
        return new BigInteger(c);
    }

    @Override
    public BigInteger negate(String x) {
        return BigInteger.ZERO.subtract(new BigInteger(x));
    }

    @Override
    BigInteger count(BigInteger x) {
        return BigInteger.valueOf(x.bitCount());
    }

}
