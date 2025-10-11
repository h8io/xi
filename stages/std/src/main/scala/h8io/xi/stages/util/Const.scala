package h8io.xi.stages.util

final case class Const[+O](out: O) extends Fn[Any, O] {
  override def f(in: Any): O = out
}
