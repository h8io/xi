package h8io.xi.stages.std

import h8io.xi.stages.{OnDone, Stage, State, Yield}

object DeadEnd extends Stage[Any, Nothing, Nothing] with OnDone[Any, Nothing, Nothing] {
  private val `yield` = Yield.None[Any, Nothing, Nothing](this)
  private val state = State.Complete[Any, Nothing, Nothing](this)

  def apply(in: Any): Yield[Any, Nothing, Nothing] = `yield`

  override def onSuccess(): State[Any, Nothing, Nothing] = state
  override def onComplete(): State[Any, Nothing, Nothing] = state
  override def onError(): State[Any, Nothing, Nothing] = state
  override def onPanic(): State[Any, Nothing, Nothing] = state

  override def dispose(): Unit = {}
}
