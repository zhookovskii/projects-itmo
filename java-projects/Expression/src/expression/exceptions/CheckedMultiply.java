package expression.exceptions;
import expression.AbstractOperation;
import expression.NewExpression;

public class CheckedMultiply extends AbstractOperation implements NewExpression {
    public CheckedMultiply(NewExpression first, NewExpression second) {
        super(first, second);
    }

    @Override
    public int evaluate(int x) {
        int f = first.evaluate(x);
        int s = second.evaluate(x);
        if (f == 0 || s == 0) {
            return 0;
        }
        if ((f == Integer.MIN_VALUE && s != 1) || (f != 1 && s == Integer.MIN_VALUE)) {
            throw new RuntimeException("overflow");
        }
        if (f > 0) {
            if ((s > 0 && f > Integer.MAX_VALUE / s) || (s < 0 && f < Integer.MIN_VALUE / s)) {
                throw new RuntimeException("overflow");
            } else {
                return f * s;
            }
        } else {
            if ((s < 0 && -f > -(Integer.MAX_VALUE / s)) || (s > 0 && -f < -(Integer.MIN_VALUE / s))) {
                throw new RuntimeException("overflow");
            } else {
                return f * s;
            }
        }
    }

    @Override
    public int evaluate(int x, int y, int z) {
        int f = first.evaluate(x, y, z);
        int s = second.evaluate(x, y, z);
        if (f == 0 || s == 0) {
            return 0;
        }
        if ((f == Integer.MIN_VALUE && s != 1) || (f != 1 && s == Integer.MIN_VALUE)) {
            throw new RuntimeException("overflow");
        }
        if (f > 0) {
            if ((s > 0 && f > Integer.MAX_VALUE / s) || (s < 0 && -f < -(Integer.MIN_VALUE / s))) {
                throw new RuntimeException("overflow");
            } else {
                return f * s;
            }
        } else {
            if ((s < 0 && -f > -(Integer.MAX_VALUE / s)) || (s > 0 && f < (Integer.MIN_VALUE / s))) {
                throw new RuntimeException("overflow");
            } else {
                return f * s;
            }
        }
    }

    @Override
    public String toString() {
        return toString("*");
    }
}
