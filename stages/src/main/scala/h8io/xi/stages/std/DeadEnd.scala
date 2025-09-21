package h8io.xi.stages.std

import h8io.xi.stages
import h8io.xi.stages.{OnDone, Stage}

object DeadEnd extends Stage.Safe[Any, Nothing, Nothing] with OnDone.Safe[Any, Nothing, Nothing] {
  val Yield = stages.Yield.None[Any, Nothing, Nothing](this)
  val State = stages.State.Complete[Any, Nothing, Nothing](this)

  def apply(in: Any): stages.Yield.None[Any, Nothing, Nothing] = Yield

  override def onSuccess(): stages.State.Complete[Any, Nothing, Nothing] = State
  override def onComplete(): stages.State.Complete[Any, Nothing, Nothing] = State
  override def onError(): stages.State.Complete[Any, Nothing, Nothing] = State
  override def onPanic(): stages.State.Complete[Any, Nothing, Nothing] = State
}
