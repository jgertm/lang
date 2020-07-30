package lang.function;

@FunctionalInterface
public interface Consumer3<A, B, C> {
  void apply3(A arg1, B arg2, C arg3);
}
