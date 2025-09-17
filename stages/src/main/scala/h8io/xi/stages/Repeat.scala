package h8io.xi.stages

import scala.annotation.tailrec

final case class Repeat[-I, +O, +E](stage: Stage[I, O, E]) extends Stage[I, O, E] {
  self =>
  override def apply(in: I): Yield[I, O, E] = {
    @tailrec def repeat(stg: Stage[I, O, E]): Yield[I, O, E] = {
      val yld = stg(in)
      yld.onDone.onSuccess() match {
        case State.Success(next) => repeat(next)
        case State.Complete(next) => yld `with` State.Success(Repeat(next)).onDone(yld.onDone.dispose _)
        case failure: State.Failure[E] => yld `with` failure.onDone(yld.onDone.dispose _)
      }
    }
    repeat(stage)
  }
}
