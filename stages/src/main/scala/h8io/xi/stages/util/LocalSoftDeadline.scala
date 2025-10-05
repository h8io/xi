package h8io.xi.stages.util

import h8io.xi.stages.{OnDone, Stage, State, Yield}

import scala.concurrent.duration.FiniteDuration

object LocalSoftDeadline {
  private[util] final case class Head[T](now: () => Long, duration: Long) extends Stage.Endo[T, Nothing] {
    assume(duration > 0, s"Duration must be positive, got duration = $duration")

    def apply(in: T): Yield.Some[T, T, Nothing] = Yield.Some(in, State.Success, OnDone.FromStage(Tail(now(), this)))
  }

  private[util] final case class Tail[T](ts: Long, head: Head[T]) extends Stage.Endo[T, Nothing] {
    self =>

    def apply(in: T): Yield.Some[T, T, Nothing] =
      if (overdue()) Yield.Some(in, State.Complete, OnDone.FromStage(head))
      else Yield.Some(
        in,
        State.Success,
        new OnDone[T, T, Nothing] {
          override def onSuccess(): Stage[T, T, Nothing] = self
          override def onComplete(): Stage[T, T, Nothing] = head
          override def onError(): Stage[T, T, Nothing] = head
        }
      )

    @inline private def overdue(): Boolean = head.now() - ts >= head.duration
  }

  def apply[T, E](duration: FiniteDuration): Stage.Endo[T, Nothing] = apply(duration.toNanos)

  def apply[T, E](duration: java.time.Duration): Stage.Endo[T, Nothing] = apply(duration.toNanos)

  @inline private def apply[T, E](duration: Long): Stage.Endo[T, Nothing] =
    if (duration > 0) Head(System.nanoTime _, duration) else DeadEnd
}
