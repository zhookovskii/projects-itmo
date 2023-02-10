package expression.generic;

import expression.handmadeExceptions.ConstOverflowException;

import java.math.BigInteger;

public class IntegerUnary extends UnaryOperation<Integer> {

    @Override
    public Integer constant(String c) {
        return BigInteger.valueOf(Long.parseLong(c)).intValue();
    }

    @Override
    public Integer negate(String x) {
        if (BigInteger.valueOf(Long.parseLong(x)).intValue() == Integer.MIN_VALUE) {
            throw new RuntimeException();
        }
        if (x.charAt(0) == '-') {
            return BigInteger.valueOf(Long.parseLong(x.substring(1))).intValue();
        } else {
            return BigInteger.valueOf(Long.parseLong("-" + x)).intValue();
        }
    }

    @Override
    Integer count(Integer x) {
        return Integer.bitCount(x);
    }

}
