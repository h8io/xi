package h8io.xi.stages.std

import h8io.xi.stages
import h8io.xi.stages.{OnDone, Stage}

sealed class DeadEnd(_dispose: () => Unit)
    extends Stage.Safe[Any, Nothing, Nothing] with OnDone.Safe[Any, Nothing, Nothing] {
  final val Yield: stages.Yield.None[Any, Nothing, Nothing] = stages.Yield.None[Any, Nothing, Nothing](this)
  final val State: stages.State.Complete[Any, Nothing, Nothing] = stages.State.Complete[Any, Nothing, Nothing](this)

  final def apply(in: Any): stages.Yield.None[Any, Nothing, Nothing] = Yield

  final def onSuccess(): stages.State.Complete[Any, Nothing, Nothing] = State
  final def onComplete(): stages.State.Complete[Any, Nothing, Nothing] = State
  final def onError(): stages.State.Complete[Any, Nothing, Nothing] = State
  final def onPanic(): stages.State.Complete[Any, Nothing, Nothing] = State

  override final def dispose(): Unit = _dispose()
}

object DeadEnd extends DeadEnd({ () => })
