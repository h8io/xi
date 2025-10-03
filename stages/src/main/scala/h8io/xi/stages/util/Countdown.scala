package h8io.xi.stages.util

import h8io.xi.stages.{OnDone, Stage, State, Yield}

object Countdown {
  private[util] final case class Impl[-I, +O, +E](i: Long, n: Long, stage: Stage[I, O, E])
      extends Stage.Decorator[I, O, E] {
    assume(n > 0, s"n must be positive, got n = $n")
    assume(0 <= i && i <= n, s"i must be in [1, $n], got i = $i")

    def apply(in: I): Yield[I, O, E] =
      if (i == 1) stage(in).map(
        identity,
        {
          case State.Success => State.Complete
          case other => other
        },
        _.map(Impl(n, n, _))
      )
      else stage(in).mapOnDone { onDone =>
        new OnDone[I, O, E] {
          def onSuccess(): Stage[I, O, E] = Impl(i - 1, n, onDone.onSuccess())
          def onComplete(): Stage[I, O, E] = Impl(n, n, onDone.onComplete())
          def onError(): Stage[I, O, E] = Impl(n, n, onDone.onError())
        }
      }
  }

  def apply[I, O, E](n: Long, stage: Stage[I, O, E]): Stage[I, O, E] =
    if (n > 0) Impl(n, n, stage) else DeadEnd(stage.dispose _)
}
