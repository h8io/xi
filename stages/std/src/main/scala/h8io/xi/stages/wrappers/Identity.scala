package h8io.xi.stages.wrappers

import h8io.xi.stages.{Alteration, Stage}

object Identity extends Alteration.Endo[Any, Nothing, Nothing] {
  def apply(stage: Stage[Any, Nothing, Nothing]): Stage[Any, Nothing, Nothing] = stage

  def apply[I, O, E]: Alteration.Endo[I, O, E] = asInstanceOf[Alteration.Endo[I, O, E]]
}
