package expression.exceptions;

import expression.AbstractOperation;
import expression.NewExpression;

public class CheckedAdd extends AbstractOperation implements NewExpression {
    public CheckedAdd(NewExpression first, NewExpression second) {
        super(first, second);
    }

    @Override
    public int evaluate(int x) {
        int f = first.evaluate(x);
        int s = second.evaluate(x);
        if ((f > 0 && s > 0 && (Integer.MAX_VALUE - s < f))
                || (f < 0 && s < 0 && (Integer.MIN_VALUE - s > f))) {
            throw new RuntimeException("overflow");
        } else {
            return f + s;
        }
    }

    @Override
    public int evaluate(int x, int y, int z) {
        int f = first.evaluate(x, y, z);
        int s = second.evaluate(x, y, z);
        if ((f > 0 && s > 0 && (Integer.MAX_VALUE - s < f))
                || (f < 0 && s < 0 && (Integer.MIN_VALUE - s > f))) {
            throw new RuntimeException("overflow");
        } else {
            return f + s;
        }
    }

    @Override
    public String toString() {
        return toString("+");
    }
}
