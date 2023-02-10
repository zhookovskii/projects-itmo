package expression;

import java.math.BigDecimal;

public class Add extends AbstractOperation implements NewExpression{

    public Add(NewExpression first, NewExpression second) {
        super(first, second);
    }

    @Override
    public int evaluate(int x) {
        return first.evaluate(x) + second.evaluate(x);
    }

    @Override
    public int evaluate(int x, int y, int z) {
        return first.evaluate(x, y, z) + second.evaluate(x, y, z);
    }

    @Override
    public String toString() {
        return toString("+");
    }

    @Override
    public double evaluate(double x, double y, double z) {
        return first.evaluate(x, y, z) + second.evaluate(x, y, z);
    }

    @Override
    public BigDecimal evaluate(BigDecimal x, BigDecimal y, BigDecimal z) {
        return first.evaluate(x, y, z).add(second.evaluate(x, y, z));
    }
}
