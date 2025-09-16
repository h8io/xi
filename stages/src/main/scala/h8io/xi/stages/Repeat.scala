package h8io.xi.stages

import scala.annotation.tailrec

final case class Repeat[-I, +O, +E](stage: Stage[I, O, E]) extends Stage[I, O, E] {
  self =>
  override def apply(in: I): Yield[I, O, E] = {
    @tailrec def repeat(stg: Stage[I, O, E]): Yield[I, O, E] = {
      val yld = stg(in)
      yld.onDone.onSuccess() match {
        case State.Success(next) => repeat(next)
        case State.Complete(next) => yld.`with`(Repeat.OnDoneFrom(State.Success(next), yld.onDone))
        // The type parameters of OnDone.Of are kept for backward compatibility with Scala 2.12
        case failure: State.Failure[E] => yld.`with`(Repeat.OnDoneFrom(failure, yld.onDone))
      }
    }
    repeat(stage)
  }
}

private object Repeat {
  private final case class OnDoneFrom[-I, +O, +E](state: State[I, O, E], onDone: OnDone[I, O, E])
      extends OnDone.Safe[I, O, E] {
    def onSuccess(): State[I, O, E] = state
    def onComplete(): State[I, O, E] = state
    def onFailure(): State[I, O, E] = state
    def dispose(): Unit = onDone.dispose()
  }
}
