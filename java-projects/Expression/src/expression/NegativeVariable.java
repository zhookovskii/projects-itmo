package expression;

import java.util.Objects;

public class NegativeVariable implements NewExpression {
    private final String var;

    public NegativeVariable(String var) {
        this.var = var;
    }

    @Override
    public int evaluate(int x, int y, int z) {
        switch (var) {
            case "x":
                return new Const(-x).evaluate(x, y, z);
            case "y":
                return new Const(-y).evaluate(x, y, z);
            case "z":
                return new Const(-z).evaluate(x, y, z);
            default:
                throw new AssertionError("Unknown variable");
        }
    }

    @Override
    public int evaluate(int x) {
        return new Const(-x).evaluate(x);
    }

    @Override
    public String toString() {
        return var;
    }

    @Override
    public boolean equals(Object o) {
        if (o instanceof Variable) {
            return hashCode() == o.hashCode();
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Objects.hash(var);
    }
}
