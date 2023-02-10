package expression.generic;

import java.util.Objects;

public abstract class AbstractOperation<T extends Number> {
    protected final TripleExpression<T> first;
    protected final TripleExpression<T> second;

    protected NarrowType<T> type;

    AbstractOperation(TripleExpression<T> first, TripleExpression<T> second, NarrowType<T> type) {
        this.first = first;
        this.second = second;
        this.type = type;
    }

    public String toString(String operation) {
        return "(" + first.toString() + " " + operation + " " + second.toString() + ")";
    }


    public boolean equals(AbstractOperation<Number> e) {
        return this.getClass() == e.getClass() && this.first == e.first && this.second == e.second;
    }

    @Override
    public int hashCode() {
        return Objects.hash(this.first, this.second, this.getClass());
    }
}
