package lang.function;

@FunctionalInterface
public interface Function1<A, R> {
  R apply1(A arg);
}
