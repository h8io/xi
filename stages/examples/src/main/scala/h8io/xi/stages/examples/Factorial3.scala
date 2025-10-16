package h8io.xi.stages.examples

import h8io.xi.stages.wrappers.Repeat
import h8io.xi.stages.{OnDone, Signal, Stage, Yield}

object Factorial3 {
  trait FactorialError

  object NegativeNumberError extends FactorialError

  final case class Factorial(i: Int, factorial: BigInt)
      extends Stage[Int, BigInt, FactorialError] with OnDone[Int, BigInt, FactorialError] {
    override def apply(in: Int): Yield[Int, BigInt, FactorialError] =
      if (in < 0) Yield.None(Signal.error(NegativeNumberError), Reset)
      else if (in < 2) Yield.Some(One, Signal.Complete, Reset)
      else if (in > i) Yield.Some(factorial, Signal.Success, this)
      else Yield.Some(factorial * i, Signal.Complete, OnDone.FromStage(InitialStage))

    override def onSuccess(): Stage[Int, BigInt, FactorialError] = Factorial(i + 1, factorial * i)
    override def onComplete(): Stage[Int, BigInt, FactorialError] = InitialStage
    override def onError(): Stage[Int, BigInt, FactorialError] = InitialStage
  }

  val InitialStage: Factorial = Factorial(2, One)

  private val Reset: OnDone[Int, BigInt, FactorialError] = OnDone.FromStage(InitialStage)

  val stage: Stage[Int, BigInt, FactorialError] = Repeat.alteration[Int, BigInt, FactorialError] <| InitialStage
}
