package h8io.xi.stages.std

final case class Const[+O](out: O) extends Fn[Any, O] {
  override def f(in: Any): O = out
}
