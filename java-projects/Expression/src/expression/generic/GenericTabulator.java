package expression.generic;

import java.math.BigDecimal;
import java.math.BigInteger;

public class GenericTabulator implements Tabulator {
    @Override
    public Object[][][] tabulate(String mode, String expression, int x1, int x2, int y1, int y2, int z1, int z2) throws Exception {
        Object[][][] tab = new Object[x2 - x1 + 1][y2 - y1 + 1][z2 - z1 + 1];
        TripleExpression<? extends Number> expr;
        switch (mode) {
            case "i":
                TripleExpression<Integer> expr1 = new ExpressionParser<Integer>().parse(expression,
                        new IntegerExpression(), new IntegerUnary());
                for (int i = 0; i <= x2 - x1; i++) {
                    for (int j = 0; j <= y2 - y1; j++) {
                        for (int k = 0; k <= z2 - z1; k++) {
                            try {
                                tab[i][j][k] = expr1.evaluate(x1 + i, y1 + j, z1 + k);
                            } catch (RuntimeException e) {
                                tab[i][j][k] = null;
                            }
                        }
                    }
                }
                break;
            case "d":
                TripleExpression<Double> expr2 = new ExpressionParser<Double>().parse(expression,
                        new DoubleExpression(), new DoubleUnary());
                for (int i = 0; i <= x2 - x1; i++) {
                    for (int j = 0; j <= y2 - y1; j++) {
                        for (int k = 0; k <= z2 - z1; k++) {
                            try {
                                tab[i][j][k] = expr2.evaluate((double) x1 + i, (double) y1 + j, (double) z1 + k);
                            } catch (RuntimeException e) {
                                tab[i][j][k] = null;
                            }
                        }
                    }
                }
                break;
            case "bi":
                TripleExpression<BigInteger> expr3 = new ExpressionParser<BigInteger>().parse(expression,
                        new BiExpression(), new BiUnary());
                for (int i = 0; i <= x2 - x1; i++) {
                    for (int j = 0; j <= y2 - y1; j++) {
                        for (int k = 0; k <= z2 - z1; k++) {
                            try {
                                tab[i][j][k] = expr3.evaluate(BigInteger.valueOf(x1 + i), BigInteger.valueOf(y1 + j),
                                        BigInteger.valueOf(z1 + k));
                            } catch (RuntimeException e) {
                                tab[i][j][k] = null;
                            }
                        }
                    }
                }
                break;
            default:
                throw new AssertionError("Unexpected evaluation mode");
        }
        return tab;
    }
}
