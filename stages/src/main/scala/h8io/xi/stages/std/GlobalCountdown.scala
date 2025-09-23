package h8io.xi.stages.std

import h8io.xi.stages.{Stage, Yield}

object GlobalCountdown {
  private[stages] final case class Impl[-I, +O, +E](i: Long, stage: Stage[I, O, E], dispose: () => Unit)
      extends Stage.Safe[I, O, E] {
    def apply(in: I): Yield[I, O, E] =
      if (i == 1) {
        val `yield` = stage.safe(in)
        `yield`.lift[I, O, E](_ => new DeadEnd(`yield`.onDone.dispose _))
      } else if (i > 0) {
        val `yield` = stage.safe(in)
        `yield`.lift(Impl(i - 1, _, `yield`.onDone.dispose _))
      } else new DeadEnd(dispose).Yield
  }

  def apply[I, O, E](n: Long, stage: Stage[I, O, E]): Stage.Safe[I, O, E] =
    if (n > 0) Impl(n, stage, { () => }) else DeadEnd
}
