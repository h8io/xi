package h8io.xi.stages.util

import h8io.xi.stages
import h8io.xi.stages.{Stage, State, Yield}

import java.time.Duration
import scala.concurrent.duration.FiniteDuration

private[util] final case class GlobalSoftDeadline[T](now: () => Long, duration: Long) extends Stage.Endo[T, Nothing] {
  self =>

  private val ts: Long = now()

  def apply(in: T): Yield.Some[T, T, Nothing] =
    Yield.Some(in, if (now() - ts < duration) State.Success else State.Complete, OnDone)

  private[util] case object OnDone extends stages.OnDone[T, T, Nothing] {
    def onSuccess(): Stage[T, T, Nothing] = self
    def onComplete(): Stage[T, T, Nothing] = self
    def onError(): Stage[T, T, Nothing] = self
  }
}

object GlobalSoftDeadline {
  def apply[T](duration: FiniteDuration): Stage.Endo[T, Nothing] =
    GlobalSoftDeadline(System.nanoTime _, duration.toNanos)

  def apply[T](duration: Duration): Stage.Endo[T, Nothing] = GlobalSoftDeadline(System.nanoTime _, duration.toNanos)
}
