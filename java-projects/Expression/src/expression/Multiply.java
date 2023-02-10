package expression;

public class Multiply extends AbstractOperation implements NewExpression {

    public Multiply(NewExpression first, NewExpression second) {
        super(first, second);
    }

    @Override
    public int evaluate(int x) {
        return first.evaluate(x) * second.evaluate(x);
    }

    @Override
    public int evaluate(int x, int y, int z) {
        return first.evaluate(x, y, z) * second.evaluate(x, y, z);
    }

    @Override
    public String toString() {
        return toString("*");
    }
}
