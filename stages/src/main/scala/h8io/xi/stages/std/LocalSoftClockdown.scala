package h8io.xi.stages.std

import h8io.xi.stages.{Stage, State, Yield}

import scala.concurrent.duration.FiniteDuration

object LocalSoftClockdown {
  private case class Initial[-I, +O, +E](duration: Long, stage: Stage[I, O, E]) extends Stage.Safe[I, O, E] {
    def apply(in: I): Yield[I, O, E] = stage.safe(in).lift(Impl(System.nanoTime(), this, _))
  }

  private case class Impl[-I, +O, +E](ts: Long, initial: Initial[I, O, E], stage: Stage[I, O, E])
      extends Stage.Safe[I, O, E] {
    override def apply(in: I): Yield[I, O, E] =
      if (System.nanoTime() - ts < initial.duration) stage.safe(in).lift(Impl(ts, initial, _))
      else Yield.None(State.Complete(initial).onDone)
  }

  def apply[I, O, E](duration: FiniteDuration, stage: Stage[I, O, E]): Stage.Safe[I, O, E] =
    apply(duration.toNanos, stage)

  def apply[I, O, E](duration: java.time.Duration, stage: Stage[I, O, E]): Stage.Safe[I, O, E] =
    apply(duration.toNanos, stage)

  @inline private def apply[I, O, E](duration: Long, stage: Stage[I, O, E]): Stage.Safe[I, O, E] =
    if (duration > 0) Initial(duration, stage) else DeadEnd
}
