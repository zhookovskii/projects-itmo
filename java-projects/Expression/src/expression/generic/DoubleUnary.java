package expression.generic;

public class DoubleUnary extends UnaryOperation<Double> {

    @Override
    public Double constant(String c) {
        return Double.parseDouble(c);
    }

    @Override
    public Double negate(String x) {
        if (x.charAt(0) == '-') {
            return Double.parseDouble(x.substring(1));
        }
        return Double.parseDouble("-" + x);
    }

    @Override
    Double count(Double x) {
        return (double) Long.bitCount(Double.doubleToLongBits(x));
    }

}
