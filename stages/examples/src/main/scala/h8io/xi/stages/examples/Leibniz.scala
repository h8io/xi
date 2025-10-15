package h8io.xi.stages.examples

import h8io.xi.stages.morphisms.{LocalSoftDeadline, Repeat}
import h8io.xi.stages.{OnDone, Signal, Stage, Yield}

import scala.concurrent.duration.FiniteDuration

object Leibniz {
  final case class Pi(n: Long, t: Double, s: Double)
      extends Stage[Unit, Double, Nothing] with OnDone[Unit, Double, Nothing] {
    def apply(in: Unit): Yield.Some[Unit, Double, Nothing] = Yield.Some(4 * s, Signal.Success, this)

    def onSuccess(): Stage[Unit, Double, Nothing] = {
      val _t = -t * (2 * n + 1) / (2 * n + 3)
      Pi(n + 1, _t, s + _t)
    }
    def onComplete(): Stage[Unit, Double, Nothing] = InitialStage
    def onError(): Stage[Unit, Double, Nothing] = InitialStage
  }

  val InitialStage: Pi = Pi(0, 1, 1)

  def stage(duration: FiniteDuration): Stage[Unit, Double, Nothing] =
    Repeat.morphism[Unit, Double, Nothing] ~>
      LocalSoftDeadline.morphism[Unit, Double, Nothing](duration) <| InitialStage
}
