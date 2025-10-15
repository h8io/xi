package h8io.xi.stages.examples

import h8io.xi.stages.morphisms.Loop
import h8io.xi.stages.std.{Const, GlobalSoftDeadline}
import h8io.xi.stages.{OnDone, Signal, Stage, Yield}

import scala.concurrent.duration.FiniteDuration

object Wallis {
  final case class Pi(n: Long) extends Stage.Endo[Double, Nothing] with OnDone[Double, Double, Nothing] {
    def apply(in: Double): Yield.Some[Double, Double, Nothing] = {
      val k = 4d * n * n
      Yield.Some(in * k / (k - 1), Signal.Success, this)
    }

    def onSuccess(): Stage[Double, Double, Nothing] = Pi(n + 1)
    def onComplete(): Stage[Double, Double, Nothing] = InitialStage
    def onError(): Stage[Double, Double, Nothing] = InitialStage
  }

  val InitialStage = Pi(1)

  def stage(duration: FiniteDuration): Stage[Any, Double, Nothing] =
    Const(2d) ~> Loop.morphism[Double, Nothing] <| InitialStage ~> GlobalSoftDeadline(duration)
}
