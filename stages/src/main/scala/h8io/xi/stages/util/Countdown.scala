package h8io.xi.stages.util

import h8io.xi.stages.{OnDone, Stage, State, Yield}

object Countdown {
  private[util] final case class Impl[T](i: Long, n: Long) extends Stage.Endo[T, Nothing] {
    assume(n > 0, s"n must be positive, got n = $n")
    assume(0 < i && i <= n, s"i must be in [1, $n], got i = $i")

    def apply(in: T): Yield.Some[T, T, Nothing] =
      if (i == 1) Yield.Some(in, State.Complete, OnDone.FromStage(Impl(n, n)))
      else Yield.Some(
        in,
        State.Success,
        new OnDone[T, T, Nothing] {
          override def onSuccess(): Stage[T, T, Nothing] = Impl(i - 1, n)
          override def onComplete(): Stage[T, T, Nothing] = Impl(n, n)
          override def onError(): Stage[T, T, Nothing] = Impl(n, n)
        }
      )
  }

  def apply[T](n: Long): Stage.Endo[T, Nothing] = if (n > 0) Impl(n, n) else DeadEnd
}
