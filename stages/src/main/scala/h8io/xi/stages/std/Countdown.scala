package h8io.xi.stages.std
import h8io.xi.stages.{OnDone, Stage, State, Yield}

object Countdown {
  private final case class Impl[T](i: Long, n: Long) extends Stage[T, T, Nothing] with OnDone[T, T, Nothing] {
    def apply(in: T): Yield[T, T, Nothing] = Yield.Some(in, this)

    def onSuccess(): State[T, T, Nothing] = if (i > 1) State.Success(Impl(i - 1, n)) else complete
    def onComplete(): State[T, T, Nothing] = complete
    def onError(): State[T, T, Nothing] = complete
    def onPanic(): State[T, T, Nothing] = complete
    def dispose(): Unit = {}

    @inline private def complete = State.Complete(Impl[T](n, n))
  }

  def apply[T](n: Long): Stage[T, T, Nothing] = Impl[T](n, n)
}
