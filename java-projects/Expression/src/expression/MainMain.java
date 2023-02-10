package expression;

public class MainMain {
    public static void main(String[] args) {
        Expression exp1 = new Add(new Variable("x"), new Variable("x"));
        Expression exp2 = new Add(new Variable("x"), new Variable("x"));
        System.out.println(exp1.equals(exp2));
    }
}
