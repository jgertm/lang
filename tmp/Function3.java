package lang.function;

@FunctionalInterface
public interface Function3<A, B, C, R> {
  R apply2(A arg1, B arg2, C arg3);
}
