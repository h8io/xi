package h8io.xi.stages.util

import h8io.xi.stages.{OnDone, Signal, Stage, Yield}

import scala.concurrent.duration.FiniteDuration

object LocalSoftDeadline {
  private[util] final case class Head[T](now: () => Long, duration: Long)
      extends Stage.Endo[T, Nothing] with OnDone[T, T, Nothing] {
    assume(duration > 0, s"Duration must be positive, got duration = $duration")

    def apply(in: T): Yield.Some[T, T, Nothing] = {
      val tail = Tail(now(), this)
      Yield.Some(in, Signal.Success, tail)
    }

    def onSuccess(): Stage[T, T, Nothing] = this
    def onComplete(): Stage[T, T, Nothing] = this
    def onError(): Stage[T, T, Nothing] = this
  }

  private[util] final case class Tail[T](ts: Long, head: Head[T])
      extends Stage.Endo[T, Nothing] with OnDone[T, T, Nothing] {
    def apply(in: T): Yield.Some[T, T, Nothing] =
      if (head.now() - ts >= head.duration) Yield.Some(in, Signal.Complete, head)
      else Yield.Some(in, Signal.Success, this)

    def onSuccess(): Stage[T, T, Nothing] = this
    def onComplete(): Stage[T, T, Nothing] = head
    def onError(): Stage[T, T, Nothing] = head
  }

  def apply[T](duration: FiniteDuration): Stage.Endo[T, Nothing] = apply(duration.toNanos)

  def apply[T](duration: java.time.Duration): Stage.Endo[T, Nothing] = apply(duration.toNanos)

  @inline private def apply[T](duration: Long): Stage.Endo[T, Nothing] =
    if (duration > 0) Head(System.nanoTime _, duration) else DeadEnd
}
