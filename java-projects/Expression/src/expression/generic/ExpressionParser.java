package expression.generic;

import expression.parser.Element;
import expression.parser.Token;
import expression.handmadeExceptions.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class ExpressionParser<T extends Number> implements TripleParser<T> {
    private String expression;
    private NarrowType<T> type;
    private UnaryOperation<T> unary;

    public TripleExpression<T> parse(String expression, NarrowType<T> type, UnaryOperation<T> unary) throws ParsingException {
        this.type = type;
        this.unary = unary;
        this.expression = expression;
        List<Element> tokens = analysis(expression);
        Iterator iterator = new Iterator(tokens);
        return expr(iterator);
    }

    private List<Element> analysis(String expression) throws ParsingException {
        Map<Character, Token> map = Map.of(
                '/', Token.DIV,
                '+', Token.PLUS,
                '-', Token.MINUS,
                '*', Token.MULTIPLY,
                '(', Token.LEFT,
                ')', Token.RIGHT,
                'x', Token.VAR,
                'y', Token.VAR,
                'z', Token.VAR
        );
        List<Element> tokens = new ArrayList<>();
        int pos = 0;
        int line = 0;
        int left_count = 0;
        int right_count = 0;
        while (pos < expression.length()) {
            if (left_count < right_count) {
                throw new BracketSequenceException("Incorrect bracket sequence at line " + (line + 1) +
                        ", position " + (pos + 1) + ": " + expression);
            }
            char curr = expression.charAt(pos);
            if (Character.toString(curr).equals(System.lineSeparator())) {
                line += 1;
                pos += 1;
                continue;
            }
            if (map.get(curr) != null) {
                tokens.add(new Element(map.get(curr), curr, pos, line));
                if (curr == '(') {
                    left_count++;
                }
                if (curr == ')') {
                    right_count++;
                }
                pos++;
            } else {
                if (Character.isDigit(curr)) {
                    StringBuilder builder = new StringBuilder();
                    do {
                        builder.append(curr);
                        pos++;
                        if (pos >= expression.length()) {
                            break;
                        }
                        curr = expression.charAt(pos);
                    } while (Character.isDigit(curr));
                    if (builder.toString().equals("2147483648")
                            && tokens.size() > 0) {
                        if (tokens.get(tokens.size() - 1).token == Token.MINUS) {
                            tokens.add(new Element(Token.NUM, builder.toString(), pos, line));
                        }
                    } else {
                        int res = Integer.parseInt(builder.toString());
                        tokens.add(new Element(Token.NUM, builder.toString(), pos, line));
                    }
                } else {
                    if (curr == 'l') {
                        if (pos + 3 > expression.length()) {
                            throw new WrongOperatorException("Instance of illegal l0 operator usage at line " +
                                    (line + 1) + ", position " + (pos + 1) + ": " + expression);
                        }
                        if (expression.charAt(pos + 1) == '0'
                                && (expression.charAt(pos + 2) == ' '
                                || expression.charAt(pos + 2) == '(')) {
                            tokens.add(new Element(Token.LEAD, "l0", pos, line));
                            pos += 2;
                        } else {
                            throw new UnexpectedArgumentException("Unexpected argument at line " + (line + 1) +
                                    ", position " + (pos + 1) + ": " + expression);
                        }
                    } else {
                        if (curr == 't') {
                            if (pos + 3 > expression.length()) {
                                throw new WrongOperatorException("Instance of illegal t0 operator usage at line " +
                                        (line + 1) + ", position " + (pos + 1) + ": " + expression);
                            }
                            if (expression.charAt(pos + 1) == '0'
                                    && (expression.charAt(pos + 2) == ' '
                                    || expression.charAt(pos + 2) == '(')) {
                                tokens.add(new Element(Token.TAIL, "t0", pos, line));
                                pos += 2;
                            } else {
                                throw new UnexpectedArgumentException("Unexpected argument at line " + (line + 1) +
                                        ", position " + (pos + 1) + ": " + expression);
                            }
                        } else {
                            if (curr == 'm') {
                                if (pos - 1 < 0 || pos + 3 >= expression.length()) {
                                    throw new WrongOperatorException("Instance of illegal min/max operator usage" +
                                            " at line " + (line + 1) + ", position " + (pos + 1) + ": " + expression);
                                }
                                if (expression.charAt(pos + 1) == 'i' && expression.charAt(pos + 2) == 'n'
                                        && (Character.isWhitespace(expression.charAt(pos + 3))
                                        || expression.charAt(pos + 3) == '('
                                        || expression.charAt(pos + 3) == '-')
                                        && (Character.isWhitespace(expression.charAt(pos - 1))
                                        || expression.charAt(pos - 1) == ')')) {
                                    tokens.add(new Element(Token.MIN, "min", pos, line));
                                    pos += 3;
                                } else {
                                    if (expression.charAt(pos + 1) == 'a' && expression.charAt(pos + 2) == 'x'
                                            && (Character.isWhitespace(expression.charAt(pos + 3))
                                            || expression.charAt(pos + 3) == '('
                                            || expression.charAt(pos + 3) == '-')
                                            && (Character.isWhitespace(expression.charAt(pos - 1))
                                            || expression.charAt(pos - 1) == ')')) {
                                        tokens.add(new Element(Token.MAX, "max", pos, line));
                                        pos += 3;
                                    } else {
                                        if (!Character.isWhitespace(curr)) {
                                            throw new UnexpectedArgumentException("Unexpected argument at line " + (line + 1) +
                                                    ", position " + (pos + 1) + ": " + expression);
                                        }
                                        pos++;
                                    }
                                }
                            } else {
                                if (curr == 'c') {
                                    tokens.add(new Element(Token.COUNT, "count", pos, line));
                                    pos+=5;
                                } else {
                                    if (!Character.isWhitespace(curr)) {
                                        throw new UnexpectedArgumentException();
                                    }
                                    pos++;
                                }
                            }
                        }
                    }
                }
            }
        }
        if (left_count != right_count) {
            throw new BracketSequenceException("Incorrect bracket sequence at line " + (line + 1) +
                    ", position " + (pos + 1) + ": " + expression);
        }
        tokens.add(new Element(Token.END, "",expression.length() - 1, line));
        /*
        for (Element token : tokens) {
            System.out.print(token.token + " ");
        }
        System.out.println();*/
        exceptionParser(tokens);
        return tokens;
    }

    public void exceptionParser(List<Element> els) throws ParsingException {
        int pos = 0;
        List<Token> binaryOperations = List.of(
                Token.PLUS, Token.DIV, Token.MULTIPLY, Token.MIN, Token.MAX
        );
        List<Token> unaryOperations = List.of(
                Token.TAIL, Token.LEAD
        );
        while (pos < els.size() - 1) {
            Token curr_token = els.get(pos).token;
            int curr_pos = els.get(pos).pos;
            int curr_line = els.get(pos).line;
            String curr_str = els.get(pos).input;
            Token next_token = els.get(pos + 1).token;
            if (curr_token == Token.NUM && next_token == Token.NUM) {
                throw new MissingOperationException("Missing operation at line " + (curr_line + 1) + ", position " +
                        (curr_pos + curr_str.length() + 1) + ": " + expression);
            }
            if (curr_token == Token.VAR && next_token == Token.VAR) {
                throw new MissingOperationException("Missing operation at line " + (curr_line + 1) + ", position " +
                        (curr_pos + curr_str.length() + 1) + ": " + expression);
            }
            if (curr_token == Token.RIGHT && next_token == Token.LEFT) {
                throw new MissingOperationException("Missing operation at line " + (curr_line + 1) + ", position " +
                        (curr_pos + curr_str.length() + 1) + ": " + expression);
            }
            boolean flag = false;
            for (Token token : binaryOperations) {
                if (token == curr_token) {
                    flag = true;
                    break;
                }
            }
            if (flag) {
                for (Token token : binaryOperations) {
                    if (token == next_token) {
                        throw new MissingArgumentException("Missing argument at line " + (curr_line + 1) +
                                ", position " + (curr_pos + curr_str.length() + 1) + ": " + expression);
                    }
                }
            }
            boolean isUnary = false;
            for (Token token : unaryOperations) {
                if (token == curr_token) {
                    isUnary = true;
                    break;
                }
            }
            if (isUnary && next_token != Token.NUM && next_token != Token.LEFT
                    && next_token != Token.MINUS && next_token != Token.VAR
                    && next_token != Token.LEAD && next_token != Token.TAIL) {
                throw new MissingArgumentException("Missing argument at line " + (curr_line + 1) +
                        ", position " + (curr_pos + curr_str.length() + 1) + ": " + expression);
            }
            pos++;
        }
    }

    private static class Iterator {
        private int pos;
        public List<Element> elements;

        public Iterator(List<Element> elements) {
            this.elements = elements;
        }

        public Element next() {
            return elements.get(pos++);
        }

        public void prev() {
            pos--;
        }

        public int getPos() {
            return pos;
        }
    }

    public TripleExpression<T> factor(Iterator iterator) throws ParsingException {
        Element element = iterator.next();
        switch (element.token) {
            case MINUS:
                Token next_token = iterator.next().token;
                iterator.prev();
                if (next_token == Token.NUM) {
                    String numVal = iterator.next().input;
                    return new CheckedConst<T>("-" + numVal, unary);
                }
                TripleExpression<T> value = factor(iterator);
                return new CheckedNegate<T>(value, unary);
            case COUNT:
                value = factor(iterator);
                return new CheckedCount<T>(value, unary);
            case VAR:
                return new Variable<T>(element.input, unary);
            case NUM:
                return new CheckedConst<T>(element.input, unary);
            case LEFT:
                value = expr(iterator);
                element = iterator.next();
                if (element.token != Token.RIGHT) {
                    throw new BracketSequenceException("Incorrect bracket sequence at line " + (element.line + 1) +
                            ", position " + (element.pos + 1) + ": " + expression);
                }
                return value;
            default:
                if (element.token == Token.END) {
                    throw new WrongOperatorException("Unexpected end of expression at line " + (element.line + 1) +
                            ", position " + (element.pos + 1) + ": " + expression);
                } else {
                    throw new WrongOperatorException("Illegal usage of operator " + element.input + " at line " +
                            (element.line + 1) + ", position " + (element.pos + 1) + ": " + expression);
                }
        }
    }

    public TripleExpression<T> multdiv(Iterator iterator) throws ParsingException {
        TripleExpression<T> value = factor(iterator);
        while (true) {
            Element element = iterator.next();
            switch (element.token) {
                case MULTIPLY:
                    value = new CheckedMultiply<T>(value, factor(iterator), type);
                    break;
                case DIV:
                    value = new CheckedDivide<T>(value, factor(iterator), type);
                    break;
                default:
                    iterator.prev();
                    return value;
            }
        }
    }

    public TripleExpression<T> plusminus(Iterator iterator) throws ParsingException {
        TripleExpression<T> value = multdiv(iterator);
        while (true) {
            Element element = iterator.next();
            switch (element.token) {
                case PLUS:
                    value = new CheckedAdd<T>(value, multdiv(iterator), type);
                    break;
                case MINUS:
                    value = new CheckedSubtract<T>(value, multdiv(iterator), type);
                    break;
                default:
                    iterator.prev();
                    return value;
            }
        }
    }

    public TripleExpression<T> expr(Iterator iterator) throws ParsingException {
        Element element = iterator.next();
        if (element.token == Token.END) {
            return new CheckedConst<T>("0", unary);
        }
        iterator.prev();
        return minMax(iterator);
    }

    public TripleExpression<T> minMax(Iterator iterator) throws ParsingException {
        TripleExpression<T> value = plusminus(iterator);
        while (true) {
            Element element = iterator.next();
            switch (element.token) {
                case MIN:
                    value = new CheckedMin<T>(value, plusminus(iterator), type);
                    break;
                case MAX:
                    value = new CheckedMax<T>(value, plusminus(iterator), type);
                    break;
                default:
                    iterator.prev();
                    return value;
            }
        }
    }
}
