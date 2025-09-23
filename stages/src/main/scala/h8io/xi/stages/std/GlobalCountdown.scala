package h8io.xi.stages.std

import h8io.xi.stages.{Stage, Yield}

object GlobalCountdown {
  private[stages] final case class Impl[-I, +O, +E](i: Long, stage: Stage[I, O, E]) extends Stage.Safe[I, O, E] {
    def apply(in: I): Yield[I, O, E] =
      if (i == 1) stage.safe(in).lift[I, O, E](_ => DeadEnd)
      else if (i > 0) stage.safe(in).lift(Impl(i - 1, _))
      else DeadEnd.Yield
  }

  def apply[I, O, E](n: Long, stage: Stage[I, O, E]): Stage.Safe[I, O, E] = if (n > 0) Impl(n, stage) else DeadEnd
}
