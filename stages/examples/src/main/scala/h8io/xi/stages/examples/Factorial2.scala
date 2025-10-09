package h8io.xi.stages.examples

import h8io.xi.stages.util.Loop
import h8io.xi.stages.{OnDone, Signal, Stage, Yield}

object Factorial2 {
  object ToTuple extends Stage.Function[Int, (Int, BigInt)] {
    override def f(in: Int): (Int, BigInt) = (in, One)
  }

  object Agg extends Stage.Endo[(Int, BigInt), Nothing] with OnDone[(Int, BigInt), (Int, BigInt), Nothing] {
    override def apply(in: (Int, BigInt)): Yield[(Int, BigInt), (Int, BigInt), Nothing] =
      if (in._1 > 1) Yield.Some((in._1 - 1, in._2 * in._1), Signal.Success, this)
      else if (in._1 < 0) Yield.None(Signal.Complete, this)
      else Yield.Some(in, Signal.Complete, this)

    override def onSuccess(): Stage[(Int, BigInt), (Int, BigInt), Nothing] = this
    override def onComplete(): Stage[(Int, BigInt), (Int, BigInt), Nothing] = this
    override def onError(): Stage[(Int, BigInt), (Int, BigInt), Nothing] = this
  }

  object Right extends Stage.Function[(Any, BigInt), BigInt] {
    override def f(in: (Any, BigInt)): BigInt = in._2
  }

  val stage: Stage[Int, BigInt, Nothing] = ToTuple ~> Loop(Agg) ~> Right
}
