package h8io.xi.stages.alterations

import h8io.xi.stages.{Decorator, Stage}

object Identity extends Decorator[Any, Nothing, Nothing] {
  def apply(stage: Stage[Any, Nothing, Nothing]): Stage[Any, Nothing, Nothing] = stage

  def apply[I, O, E]: Decorator[I, O, E] = asInstanceOf[Decorator[I, O, E]]
}
