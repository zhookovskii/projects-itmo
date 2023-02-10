package expression.exceptions;

import expression.AbstractOperation;
import expression.NewExpression;

public class CheckedDivide extends AbstractOperation implements NewExpression {
    public CheckedDivide(NewExpression first, NewExpression second) {
        super(first, second);
    }

    @Override
    public int evaluate(int x) {
        if (second.evaluate(x) == 0) {
            throw new RuntimeException("division by zero");
        } else {
            if (first.evaluate(x) == Integer.MIN_VALUE && second.evaluate(x) == -1) {
                throw new RuntimeException("overflow");
            } else {
                return first.evaluate(x) / second.evaluate(x);
            }
        }
    }

    @Override
    public int evaluate(int x, int y, int z) {
        if (second.evaluate(x, y, z) == 0) {
            throw new RuntimeException("division by zero");
        } else {
            if (first.evaluate(x, y, z) == Integer.MIN_VALUE && second.evaluate(x, y, z) == -1) {
                throw new RuntimeException("overflow");
            } else {
                return first.evaluate(x, y, z) / second.evaluate(x, y, z);
            }
        }
    }

    @Override
    public String toString() {
        return toString("/");
    }
}
