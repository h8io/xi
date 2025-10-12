package h8io.xi.stages.examples

import h8io.xi.stages.decorators.Repeat
import h8io.xi.stages.{OnDone, Signal, Stage, Yield}

object Factorial3 {
  trait FactorialError

  object NegativeNumberError extends FactorialError

  final case class Agg(i: Int, factorial: BigInt) extends Stage[Int, BigInt, FactorialError] {
    override def apply(in: Int): Yield[Int, BigInt, FactorialError] =
      if (in < 0) Yield.None(Signal.error(NegativeNumberError), OnDone.FromStage(InitialAgg))
      else if (in < 2) Yield.Some(One, Signal.Complete, OnDone.FromStage(InitialAgg))
      else if (in > i) Yield.Some(
        factorial,
        Signal.Success,
        new OnDone[Int, BigInt, FactorialError] {
          override def onSuccess(): Stage[Int, BigInt, FactorialError] = Agg(i + 1, factorial * i)
          override def onComplete(): Stage[Int, BigInt, FactorialError] = InitialAgg
          override def onError(): Stage[Int, BigInt, FactorialError] = InitialAgg
        }
      )
      else Yield.Some(factorial * i, Signal.Complete, OnDone.FromStage(InitialAgg))
  }

  private val InitialAgg: Agg = Agg(2, One)

  val stage: Repeat[Int, BigInt, FactorialError] = Repeat(InitialAgg)
}
