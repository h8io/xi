package h8io.xi.stages.util

import h8io.xi.stages.Fn

final case class Const[+O](out: O) extends Fn[Any, O] {
  override def f(in: Any): O = out
}
