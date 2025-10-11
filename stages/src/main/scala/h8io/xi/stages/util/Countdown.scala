package h8io.xi.stages.util

import h8io.xi.stages.{Fruitful, OnDone, Signal, Stage, Yield}

final case class Countdown[T](i: Long, n: Long) extends Fruitful.Endo[T, Nothing] with OnDone[T, T, Nothing] {
  assume(n > 0, s"n must be positive, got n = $n")
  assume(0 < i && i <= n, s"i must be in [1, $n], got i = $i")

  def apply(in: T): Yield.Some[T, T, Nothing] =
    if (i == 1) Yield.Some(in, Signal.Complete, OnDone.FromStage(Countdown(n, n)))
    else Yield.Some(in, Signal.Success, this)

  def onSuccess(): Stage[T, T, Nothing] = Countdown(i - 1, n)
  def onComplete(): Stage[T, T, Nothing] = Countdown(n, n)
  def onError(): Stage[T, T, Nothing] = Countdown(n, n)
}

object Countdown {
  def apply[T](n: Long): Stage.Endo[T, Nothing] = if (n > 0) Countdown(n, n) else DeadEnd
}
