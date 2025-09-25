package h8io.xi.stages.util

import h8io.xi.stages.{Stage, Yield}

object GlobalCountdown {
  private[util] final case class Impl[-I, +O, +E](i: Long, stage: Stage[I, O, E]) extends Stage.Safe[I, O, E] {
    def apply(in: I): Yield[I, O, E] = {
      val `yield` = stage.safe(in)
      `yield`.lift[I, O, E](if (i == 1) _ => new DeadEnd(`yield`.onDone.dispose _) else Impl(i - 1, _))
    }
  }

  def apply[I, O, E](n: Long, stage: Stage[I, O, E]): Stage.Safe[I, O, E] = if (n > 0) Impl(n, stage) else DeadEnd
}
