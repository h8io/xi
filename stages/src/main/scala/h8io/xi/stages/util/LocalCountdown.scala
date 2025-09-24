package h8io.xi.stages.util

import h8io.xi.stages.{OnDone, Stage, State, Yield}

object LocalCountdown {
  private[stages] final case class Impl[-I, +O, +E](i: Long, n: Long, stage: Stage[I, O, E])
      extends Stage.Safe[I, O, E] {
    def apply(in: I): Yield[I, O, E] =
      if (i == 1) stage.safe(in).complete(Impl(n, n, _))
      else if (i > 1) stage.safe(in).map { onDone =>
        new OnDone[I, O, E] {
          def onSuccess(): State[I, O, E] = onDone.onSuccess().map(Impl(i - 1, n, _))
          def onComplete(): State[I, O, E] = onDone.onComplete().map(Impl(n, n, _))
          def onError(): State[I, O, E] = onDone.onError().map(Impl(n, n, _))
          def onPanic(): State[I, O, E] = onDone.onPanic().map(Impl(n, n, _))

          override def dispose(): Unit = onDone.dispose()
        }
      }
      else Yield.None(State.Complete(Impl(n, n, stage)).onDone)
  }

  def apply[I, O, E](n: Long, stage: Stage[I, O, E]): Stage.Safe[I, O, E] = if (n > 0) Impl(n, n, stage) else DeadEnd
}
