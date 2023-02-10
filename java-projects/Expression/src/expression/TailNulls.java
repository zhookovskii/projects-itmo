package expression;

public class TailNulls implements NewExpression {
    private final NewExpression expr;

    public TailNulls(NewExpression expr) {
        this.expr = expr;
    }

    @Override
    public int evaluate(int x) {
        return Integer.numberOfTrailingZeros(expr.evaluate(x));
    }

    @Override
    public int evaluate(int x, int y, int z) {
        return Integer.numberOfTrailingZeros(expr.evaluate(x, y, z));
    }

    @Override
    public String toString() {
        return "t0(" + expr.toString() + ")";
    }

    @Override
    public boolean equals(Object o) {
        if (o instanceof TailNulls) {
            return expr.equals(o);
        }
        return false;
    }
}

