package h8io.xi.stages.util

import h8io.xi.stages
import h8io.xi.stages.{OnDone, Signal, Stage}

sealed case class DeadEnd(_dispose: () => Unit)
    extends Stage[Any, Nothing, Nothing] with OnDone[Any, Nothing, Nothing] {
  final val Yield: stages.Yield.None[Any, Nothing, Nothing] =
    stages.Yield.None[Any, Nothing, Nothing](Signal.Complete, this)

  final def apply(in: Any): stages.Yield.None[Any, Nothing, Nothing] = Yield

  final def onSuccess(): Stage[Any, Nothing, Nothing] = this
  final def onComplete(): Stage[Any, Nothing, Nothing] = this
  final def onError(): Stage[Any, Nothing, Nothing] = this

  override final def dispose(): Unit = _dispose()
}

object DeadEnd extends DeadEnd({ () => })
