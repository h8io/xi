package h8io.xi.stages.util

import h8io.xi.stages
import h8io.xi.stages.{Stage, State, Yield}

import java.time.{Duration, Instant}
import scala.concurrent.duration.FiniteDuration

private[util] final case class GlobalSoftDeadline[T](now: () => Instant, deadline: Instant)
    extends Stage.Endo[T, Nothing] {
  self =>

  def apply(in: T): Yield.Some[T, T, Nothing] =
    Yield.Some(in, if (deadline.isAfter(now())) State.Success else State.Complete, OnDone)

  private[util] case object OnDone extends stages.OnDone[T, T, Nothing] {
    def onSuccess(): Stage[T, T, Nothing] = self
    def onComplete(): Stage[T, T, Nothing] = self
    def onError(): Stage[T, T, Nothing] = self
  }
}

object GlobalSoftDeadline {
  def apply[T](deadline: Instant): Stage.Endo[T, Nothing] = GlobalSoftDeadline(Instant.now _, deadline)

  def apply[T](duration: FiniteDuration): Stage.Endo[T, Nothing] =
    GlobalSoftDeadline(Instant.now _, Instant.now().plusNanos(duration.toNanos))

  def apply[T](duration: Duration): Stage.Endo[T, Nothing] =
    GlobalSoftDeadline(Instant.now _, Instant.now().plus(duration))
}
