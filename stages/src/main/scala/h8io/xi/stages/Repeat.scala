package h8io.xi.stages

import scala.annotation.tailrec

final case class Repeat[-I, +O, +E](stage: Stage[I, O, E]) extends Stage[I, O, E] {
  self =>
  override def apply(in: I): Yield[I, O, E] = {
    @tailrec def repeat(stg: Stage[I, O, E]): Yield[I, O, E] = {
      val yld = stg(in)
      yld.onDone.onSuccess() match {
        case State.Success(next) => repeat(next)
        case State.Complete(next) => yld `with` OnDone.Stable(State.Success(Repeat(next)), yld.onDone.dispose _)
        // The type parameters of OnDone.Of are kept for backward compatibility with Scala 2.12
        case failure: State.Failure[E] => yld `with` OnDone.Stable[I, O, E](failure, yld.onDone.dispose _)
      }
    }
    repeat(stage)
  }
}
