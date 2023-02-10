package expression;

import java.math.BigInteger;

public class NegativeConst implements NewExpression {
    private final String c;

    public NegativeConst(String c) {
        this.c = c;
    }

    @Override
    public int evaluate(int x) {
        return -BigInteger.valueOf(Long.parseLong(c)).intValue();
    }

    @Override
    public int evaluate(int x, int y, int z) {
        return -BigInteger.valueOf(Long.parseLong(c)).intValue();
    }

    @Override
    public String toString() {
        return "-" + c;
    }

    @Override
    public boolean equals(Object o) {
        if (o instanceof Const) {
            return o.hashCode() == this.hashCode();
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Integer.hashCode(-Integer.parseInt(c));
    }
}
