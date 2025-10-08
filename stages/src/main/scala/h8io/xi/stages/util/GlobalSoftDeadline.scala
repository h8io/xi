package h8io.xi.stages.util

import h8io.xi.stages.{OnDone, Signal, Stage, Yield}

import java.time.Duration
import scala.concurrent.duration.FiniteDuration

private[util] final case class GlobalSoftDeadline[T](now: () => Long, duration: Long)
    extends Stage.Endo[T, Nothing] with OnDone[T, T, Nothing] {
  private val ts: Long = now()

  def apply(in: T): Yield.Some[T, T, Nothing] =
    Yield.Some(in, if (now() - ts < duration) Signal.Success else Signal.Complete, this)

  def onSuccess(): Stage[T, T, Nothing] = this
  def onComplete(): Stage[T, T, Nothing] = this
  def onError(): Stage[T, T, Nothing] = this
}

object GlobalSoftDeadline {
  def apply[T](duration: FiniteDuration): Stage.Endo[T, Nothing] =
    GlobalSoftDeadline(System.nanoTime _, duration.toNanos)

  def apply[T](duration: Duration): Stage.Endo[T, Nothing] = GlobalSoftDeadline(System.nanoTime _, duration.toNanos)
}
