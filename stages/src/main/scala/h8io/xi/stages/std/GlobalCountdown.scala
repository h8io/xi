package h8io.xi.stages.std

import h8io.xi.stages.{Stage, Yield}

final case class GlobalCountdown[-I, +O, +E](i: Long, stage: Stage[I, O, E]) extends Stage.Safe[I, O, E] {
  def apply(in: I): Yield[I, O, E] = {
    if (i == 0) DeadEnd(in)
    else stage.safe(in).lift(GlobalCountdown(i - 1, _))
  }
}
