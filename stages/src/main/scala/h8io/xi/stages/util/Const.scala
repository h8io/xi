package h8io.xi.stages.util

import h8io.xi.stages.Stage

final case class Const[+O](out: O) extends Stage.Function[Any, O] {
  override def f(in: Any): O = out
}
