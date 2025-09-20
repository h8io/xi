package h8io.xi.stages.std

import h8io.xi.stages.{Stage, State, Yield}

object Countdown {
  private final case class Impl[I, O, E](i: Long, n: Long, global: Boolean, stage: Stage[I, O, E])
      extends Stage[I, O, E] {
    def apply(in: I): Yield[I, O, E] = {
      if (i == 0) Yield.None(State.Success(this).onDone(() => {}))
      else if (i == 1) stage(in).complete(stage => Impl(n, n, global, stage))
      else stage(in).lift(stage => Impl(n, n, global, stage))
    }
  }

  def apply[I, O, E](n: Long, global: Boolean, stage: Stage[I, O, E]): Stage[I, O, E] = Impl(n, n, global, stage)
}
