package h8io.xi.stages.std

import h8io.xi.stages.{Stage, State, Yield}

object Countdown {
  private[stages] final case class Impl[I, O, E](i: Long, n: Long, stage: Stage[I, O, E]) extends Stage[I, O, E] {
    def apply(in: I): Yield[I, O, E] =
      if (i == 1) stage(in).complete(Impl(n, n, _))
      else if (i > 1) stage(in).lift(Impl(i - 1, n, _))
      else Yield.None(State.Complete(Impl(n, n, stage)).onDone(() => {}))
  }

  def apply[I, O, E](n: Long, stage: Stage[I, O, E]): Stage[I, O, E] = Impl(n, n, stage)
}
