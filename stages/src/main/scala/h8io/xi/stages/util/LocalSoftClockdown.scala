package h8io.xi.stages.util

import h8io.xi.stages.{Stage, State, Yield}

import scala.concurrent.duration.FiniteDuration

object LocalSoftClockdown {
  private[stages] final case class Head[-I, +O, +E](now: () => Long, duration: Long, stage: Stage[I, O, E])
      extends Stage.Safe[I, O, E] {
    def apply(in: I): Yield[I, O, E] = if (duration > 0) stage.safe(in).lift(Tail(now(), this, _)) else DeadEnd.Yield
  }

  private[stages] final case class Tail[-I, +O, +E](ts: Long, head: Head[I, O, E], stage: Stage[I, O, E])
      extends Stage.Safe[I, O, E] {
    override def apply(in: I): Yield[I, O, E] =
      if (head.now() - ts < head.duration) stage.safe(in).lift(Tail(ts, head, _))
      else Yield.None(State.Complete(head).onDone)
  }

  def apply[I, O, E](duration: FiniteDuration, stage: Stage[I, O, E]): Stage.Safe[I, O, E] =
    apply(duration.toNanos, stage)

  def apply[I, O, E](duration: java.time.Duration, stage: Stage[I, O, E]): Stage.Safe[I, O, E] =
    apply(duration.toNanos, stage)

  @inline private def apply[I, O, E](duration: Long, stage: Stage[I, O, E]): Stage.Safe[I, O, E] =
    if (duration > 0) Head(System.nanoTime _, duration, stage) else DeadEnd
}
