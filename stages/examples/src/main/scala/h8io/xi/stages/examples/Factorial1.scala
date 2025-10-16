package h8io.xi.stages.examples

import h8io.xi.stages.wrappers.Loop
import h8io.xi.stages.std.{Const, Countdown}
import h8io.xi.stages.{OnDone, Signal, Stage, Yield}

object Factorial1 {
  sealed case class Agg(n: Long) extends Stage.Endo[BigInt, Nothing] with OnDone[BigInt, BigInt, Nothing] {
    override def apply(in: BigInt): Yield.Some[BigInt, BigInt, Nothing] = Yield.Some(in * n, Signal.Success, this)

    override def onSuccess(): Stage.Endo[BigInt, Nothing] = Agg(n + 1)
    override def onComplete(): Stage.Endo[BigInt, Nothing] = Agg
    override def onError(): Stage.Endo[BigInt, Nothing] = Agg
  }

  object Agg extends Agg(1)

  def stage(n: Int): Stage[Unit, BigInt, Nothing] =
    Const(One) ~> Loop.alteration[BigInt, Nothing] <| Agg ~> Countdown[BigInt](n)
}
