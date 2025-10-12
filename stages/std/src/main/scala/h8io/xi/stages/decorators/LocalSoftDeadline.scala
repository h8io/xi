package h8io.xi.stages.decorators

import h8io.xi.stages.std.DeadEnd
import h8io.xi.stages.{OnDone, Signal, Stage, Yield}

import scala.concurrent.duration.FiniteDuration

trait LocalSoftDeadline[-I, +O, +E] extends Decorator[I, O, E]

object LocalSoftDeadline {
  private[decorators] final case class Head[-I, +O, +E](now: () => Long, duration: Long, stage: Stage[I, O, E])
      extends LocalSoftDeadline[I, O, E] {
    assume(duration > 0, s"Duration must be positive, got duration = $duration")

    def apply(in: I): Yield[I, O, E] = {
      val ts = now()
      stage(in).mapOnDone(_OnDone(ts, now, duration, _))
    }
  }

  private[decorators] final case class Tail[-I, +O, +E](
      ts: Long,
      now: () => Long,
      duration: Long,
      stage: Stage[I, O, E])
      extends LocalSoftDeadline[I, O, E] {
    def apply(in: I): Yield[I, O, E] =
      if (now() - ts >= duration) Yield.None(Signal.Complete, OnDone.FromStage(Head(now, duration, stage)))
      else stage(in).mapOnDone(_OnDone(ts, now, duration, _))
  }

  private[decorators] final case class _OnDone[-I, +O, +E](
      ts: Long,
      now: () => Long,
      duration: Long,
      onDone: OnDone[I, O, E])
      extends OnDone[I, O, E] {
    def onSuccess(): Stage[I, O, E] = Tail(ts, now, duration, onDone.onSuccess())
    def onComplete(): Stage[I, O, E] = Head(now, duration, onDone.onComplete())
    def onError(): Stage[I, O, E] = Head(now, duration, onDone.onError())
  }

  def apply[I, O, E](duration: FiniteDuration, stage: Stage[I, O, E]): Stage[I, O, E] = apply(duration.toNanos, stage)

  def apply[I, O, E](duration: java.time.Duration, stage: Stage[I, O, E]): Stage[I, O, E] =
    apply(duration.toNanos, stage)

  @inline private def apply[I, O, E](duration: Long, stage: Stage[I, O, E]): Stage[I, O, E] =
    if (duration > 0) Head(System.nanoTime _, duration, stage) else DeadEnd
}
