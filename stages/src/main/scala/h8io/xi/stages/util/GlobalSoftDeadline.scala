package h8io.xi.stages.util

import h8io.xi.stages
import h8io.xi.stages.{Stage, State, Yield}

import java.time.Instant
import scala.math.Ordering.Implicits.*

final case class GlobalSoftDeadline[T](deadline: Instant) extends Stage.Endo[T, Nothing] {
  self =>

  def apply(in: T): Yield.Some[T, T, Nothing] =
    Yield.Some(in, if (deadline > Instant.now()) State.Success else State.Complete, OnDone)

  private[util] case object OnDone extends stages.OnDone[T, T, Nothing] {
    def onSuccess(): Stage[T, T, Nothing] = self
    def onComplete(): Stage[T, T, Nothing] = self
    def onError(): Stage[T, T, Nothing] = self
  }
}
