package h8io.xi.stages.std
import h8io.xi.stages.Stage

object Identity extends Stage.Endo[Any, Nothing] with Fn[Any, Any] with Stage.Static[Any, Any, Nothing] {
  def apply[T]: Fn[T, Nothing] = asInstanceOf[Fn[T, Nothing]]

  def f(in: Any): Any = in
}
