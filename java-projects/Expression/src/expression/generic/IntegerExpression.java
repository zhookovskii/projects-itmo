package expression.generic;

public class IntegerExpression extends NarrowType<Integer>{

    @Override
    public Integer add(Integer x, Integer y) {
        int f = x;
        int s = y;
        if ((f > 0 && s > 0 && (java.lang.Integer.MAX_VALUE - s < f))
                || (f < 0 && s < 0 && (java.lang.Integer.MIN_VALUE - s > f))) {
            throw new RuntimeException("overflow");
        } else {
            return f + s;
        }
    }

    @Override
    public Integer subtract(Integer x, Integer y) {
        int f = x;
        int s = y;
        if ((f > 0 && s < 0 && (Integer.MAX_VALUE + s < f))
                || (f < 0 && s > 0 && (Integer.MIN_VALUE + s > f))) {
            throw new RuntimeException("overflow");
        } else {
            if (f == 0 && s == Integer.MIN_VALUE) {
                throw new RuntimeException("overflow");
            } else {
                return f - s;
            }
        }
    }

    @Override
    public Integer multiply(Integer x, Integer y) {
        int f = x;
        int s = y;
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
    public Integer divide(Integer x, Integer y) {
        if (y == 0) {
            throw new RuntimeException("division by zero");
        } else {
            if (x == Integer.MIN_VALUE && y == -1) {
                throw new RuntimeException("overflow");
            } else {
                return x / y;
            }
        }
    }

    @Override
    Integer min(Integer x, Integer y) {
        return Math.min(x, y);
    }

    @Override
    Integer max(Integer x, Integer y) {
        return Math.max(x, y);
    }
}
