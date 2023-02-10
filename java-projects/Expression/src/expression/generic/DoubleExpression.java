package expression.generic;

public class DoubleExpression extends NarrowType<Double> {

    @Override
    public Double add(Double x, Double y) {
        double f = x;
        double s = y;
        return f + s;
    }

    @Override
    public Double subtract(Double x, Double y) {
        double f = x;
        double s = y;
        return f - s;
    }

    @Override
    public Double multiply(Double x, Double y) {
        double f = x;
        double s = y;
        return f * s;
    }

    @Override
    public Double divide(Double x, Double y) {
        double f = x;
        double s = y;
        return f / s;
    }

    @Override
    Double min(Double x, Double y) {
        return Math.min(x, y);
    }

    @Override
    Double max(Double x, Double y) {
        return Math.max(x, y);
    }
}
