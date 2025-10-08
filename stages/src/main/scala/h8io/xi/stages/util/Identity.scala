package h8io.xi.stages.util

import h8io.xi.stages.Stage

object Identity extends Stage.Endo[Any, Nothing] with Stage.Function[Any, Any] {
  def apply[T]: Stage.Endo[T, Nothing] = asInstanceOf[Stage.Endo[T, Nothing]]

  override def f(in: Any): Any = in
}
