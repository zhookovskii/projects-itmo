package expression.exceptions;

import expression.Const;
import expression.Negate;
import expression.NewExpression;
import expression.TripleExpression;

public class Bowser {
    public static void main(String[] args) throws ParsingException {
        ExpressionParser parser = new ExpressionParser();
        String expr = "(0 \u000B\f\u000B\n" +
                "\f\n" +
                "\f)+ 1)";
        String expr2 = "min";
        TripleExpression parsed = parser.parse(expr2);
        System.out.println(parsed.evaluate(1, 2, 3));
    }
}
