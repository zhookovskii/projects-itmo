package expression.exceptions;
import expression.Const;
import expression.NewExpression;

public class CheckedNegate implements NewExpression {
    private final NewExpression expr;

    public CheckedNegate(NewExpression expr) {
        this.expr = expr;
    }

    @Override
    public int evaluate(int x) {
        int res = expr.evaluate(x);
        if (expr instanceof Const) {
            return Integer.parseInt("-" + expr.toString());
        }
        if (res == Integer.MIN_VALUE) {
            throw new RuntimeException("overflow");
        } else {
            return -expr.evaluate(x);
        }
    }

    @Override
    public int evaluate(int x, int y, int z) {
        int res = expr.evaluate(x, y, z);
        if (expr instanceof Const) {
            return Integer.parseInt("-" + expr.toString());
        }
        if (res == Integer.MIN_VALUE) {
            throw new RuntimeException("overflow");
        } else {
            return -expr.evaluate(x, y, z);
        }
    }

    @Override
    public String toString() {
        return "-" + expr.toString();
    }

    @Override
    public boolean equals(Object o) {
        return expr.equals(o);
    }
}

