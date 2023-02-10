package expression;

public class Negate implements NewExpression {
    private final NewExpression expr;

    public Negate(NewExpression expr) {
        this.expr = expr;
    }

    @Override
    public int evaluate(int x) {
        return -expr.evaluate(x);
    }

    @Override
    public int evaluate(int x, int y, int z) {
        return -expr.evaluate(x, y, z);
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
