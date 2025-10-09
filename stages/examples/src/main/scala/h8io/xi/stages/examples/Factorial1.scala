package h8io.xi.stages.examples

import h8io.xi.stages.util.{Const, Countdown, Loop}
import h8io.xi.stages.{OnDone, Signal, Stage, Yield}

object Factorial1 {
  sealed case class Agg(n: Long) extends Stage.Endo[BigInt, Nothing] with OnDone[BigInt, BigInt, Nothing] {
    override def apply(in: BigInt): Yield.Some[BigInt, BigInt, Nothing] = Yield.Some(in * n, Signal.Success, this)

    override def onSuccess(): Stage[BigInt, BigInt, Nothing] = Agg(n + 1)
    override def onComplete(): Stage[BigInt, BigInt, Nothing] = Agg
    override def onError(): Stage[BigInt, BigInt, Nothing] = Agg
  }

  object Agg extends Agg(1)

  def stage(n: Int): Stage[Unit, BigInt, Nothing] = Const(BigInt.apply(1)) ~> Loop(Agg ~> Countdown[BigInt](n))
}
