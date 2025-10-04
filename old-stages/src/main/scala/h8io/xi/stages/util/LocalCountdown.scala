package h8io.xi.stages.util

import h8io.xi.stages.{OnDone, Stage, State, Yield}

object LocalCountdown {
  private[util] final case class Impl[-I, +O, +E](i: Long, n: Long, stage: Stage[I, O, E]) extends Stage.Safe[I, O, E] {
    assume(n > 0, s"n must be positive, got n = $n")
    assume(0 <= i && i <= n, s"i must be in [0, $n], got i = $i")

    def apply(in: I): Yield[I, O, E] =
      if (i == 1) stage.safe(in).complete(Impl(n, n, _))
      else stage.safe(in).map { onDone =>
        new OnDone[I, O, E] {
          def onSuccess(): State[I, O, E] = onDone.onSuccess().map(Impl(i - 1, n, _))
          def onComplete(): State[I, O, E] = onDone.onComplete().complete(Impl(n, n, _))
          def onError(): State[I, O, E] = onDone.onError().complete(Impl(n, n, _))
          def onPanic(): State[I, O, E] = onDone.onPanic().complete(Impl(n, n, _))

          override def dispose(): Unit = onDone.dispose()
        }
      }
  }

  def apply[I, O, E](n: Long, stage: Stage[I, O, E]): Stage.Safe[I, O, E] = if (n > 0) Impl(n, n, stage) else DeadEnd
}
