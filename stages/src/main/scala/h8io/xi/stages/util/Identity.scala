package h8io.xi.stages.util

import h8io.xi.stages.{OnDone, Stage, State, Yield}

object Identity extends Stage.SafeEndo[Any, Nothing] {
  private val onDone: OnDone.SafeEndo[Any, Nothing] = State.Success[Any, Any, Nothing](this).onDone

  def apply[T]: Stage.SafeEndo[T, Nothing] = this.asInstanceOf[Stage.SafeEndo[T, Nothing]]

  def apply(in: Any): Yield.Endo[Any, Nothing] = Yield.Some(in, onDone)
}
