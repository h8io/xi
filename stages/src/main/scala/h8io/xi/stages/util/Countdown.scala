package h8io.xi.stages.util

import h8io.xi.stages.{OnDone, Stage, State, Yield}

private[util] final case class Countdown[T](i: Long, n: Long) extends Stage.Endo[T, Nothing] {
  assume(n > 0, s"n must be positive, got n = $n")
  assume(0 < i && i <= n, s"i must be in [1, $n], got i = $i")

  def apply(in: T): Yield.Some[T, T, Nothing] =
    if (i == 1) Yield.Some(in, State.Complete, OnDone.FromStage(Countdown(n, n)))
    else Yield.Some(
      in,
      State.Success,
      new OnDone[T, T, Nothing] {
        override def onSuccess(): Stage[T, T, Nothing] = Countdown(i - 1, n)
        override def onComplete(): Stage[T, T, Nothing] = Countdown(n, n)
        override def onError(): Stage[T, T, Nothing] = Countdown(n, n)
      }
    )
}

object Countdown {
  def apply[T](n: Long): Stage.Endo[T, Nothing] = if (n > 0) Countdown(n, n) else DeadEnd
}
