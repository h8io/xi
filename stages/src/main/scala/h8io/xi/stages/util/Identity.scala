package h8io.xi.stages.util

import h8io.xi.stages
import h8io.xi.stages.{Signal, Stage, Yield}

object Identity extends Stage.Endo[Any, Nothing] {
  private val OnDone = stages.OnDone.FromStage[Any, Any, Nothing](this)

  def apply[T]: Stage.Endo[T, Nothing] = asInstanceOf[Stage.Endo[T, Nothing]]

  def apply(in: Any): Yield[Any, Any, Nothing] = Yield.Some(in, Signal.Success, OnDone)
}
