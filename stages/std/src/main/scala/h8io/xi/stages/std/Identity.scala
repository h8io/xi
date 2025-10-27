package h8io.xi.stages.std

object Identity extends Fn.Endo[Any] {
  def apply[T]: Fn[T, Nothing] = asInstanceOf[Fn[T, Nothing]]

  def f(in: Any): Any = in
}
