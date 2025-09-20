package h8io.xi.stages.std

import h8io.xi.stages.{OnDone, Stage, State, Yield}

object Countdown {
  private[stages] final case class Impl[I, O, E](i: Long, n: Long, stage: Stage[I, O, E]) extends Stage[I, O, E] {
    def apply(in: I): Yield[I, O, E] =
      if (i == 1) stage.safe(in).complete(reset)
      else if (i > 1) stage.safe(in).map { onDone =>
        new OnDone[I, O, E] {
          def onSuccess(): State[I, O, E] = onDone.onSuccess().map(Impl(i - 1, n, _))
          def onComplete(): State[I, O, E] = onDone.onComplete().map(reset)
          def onError(): State[I, O, E] = onDone.onError().map(reset)
          def onPanic(): State[I, O, E] = onDone.onPanic().map(reset)

          override def dispose(): Unit = onDone.dispose()
        }
      }
      else Yield.None(State.Complete(Impl(n, n, stage)).onDone(() => {}))

    @inline private def reset(stage: Stage[I, O, E]): Impl[I, O, E] = Impl(n, n, stage)
  }

  def apply[I, O, E](n: Long, stage: Stage[I, O, E]): Stage[I, O, E] = Impl(n, n, stage)
}
