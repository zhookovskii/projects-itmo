package expression.generic;

import expression.exceptions.ParsingException;

import java.math.BigInteger;

public class Mario {
    public static void main(String[] args) throws ParsingException, expression.handmadeExceptions.ParsingException {
/*
        TripleExpression<Double> expr = new ExpressionParser<Double>().parse("-1", new DoubleExpression(),
                new DoubleUnary());

        TripleExpression<Integer> intexpr = new ExpressionParser<Integer>().parse("-2147483648",
                new IntegerExpression(), new IntegerUnary());

 */
        TripleExpression<Double> intexpr1 = new ExpressionParser<Double>().parse("3.6 + 5.6",
                new DoubleExpression(), new DoubleUnary());
        //System.out.println(intexpr.evaluate(-2147483646, -9, -1));
        System.out.println(intexpr1.evaluate(0.25, 0.12345, 0.32456));
    }
}
