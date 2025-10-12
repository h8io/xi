package h8io.xi.stages.decorators

import h8io.xi.stages.decorators.LocalSoftDeadline.{_OnDone, Head}
import h8io.xi.stages.std.DeadEnd
import h8io.xi.stages.{OnDone, Stage, Yield}

import scala.concurrent.duration.FiniteDuration

sealed trait LocalSoftDeadline[-I, +O, +E] extends Decorator[I, O, E] {
  val now: () => Long
  val duration: Long

  @inline protected final def overdue(ts: Long): Boolean = now() - ts >= duration

  @inline protected final def apply(ts: Long, in: I): Yield[I, O, E] = {
    val `yield` = stage(in)
    if (overdue(ts)) `yield`.mapOnDoneAndComplete(_.map(Head(now, duration, _)))
    else `yield`.mapOnDone(_OnDone(ts, now, duration, _))
  }
}

object LocalSoftDeadline {
  private[decorators] final case class Head[-I, +O, +E](now: () => Long, duration: Long, stage: Stage[I, O, E])
      extends LocalSoftDeadline[I, O, E] {
    assume(duration > 0, s"Duration must be positive, got duration = $duration")

    def apply(in: I): Yield[I, O, E] = apply(now(), in)
  }

  private[decorators] final case class Tail[-I, +O, +E](
      ts: Long,
      now: () => Long,
      duration: Long,
      stage: Stage[I, O, E])
      extends LocalSoftDeadline[I, O, E] {
    def apply(in: I): Yield[I, O, E] =
      if (overdue(ts)) stage(in).mapOnDoneAndComplete(_.map(Head(now, duration, _))) else apply(ts, in)
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
