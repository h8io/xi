package h8io.xi.stages

import scala.annotation.tailrec

final case class Repeat[-I, +O, +E](stage: Stage[I, O, E]) extends Stage[I, O, E] {
  self =>

  override def apply(in: I): Yield[I, O, E] = {
    @tailrec def repeat(stg: Stage[I, O, E]): Yield[I, O, E] = {
      val outcome = stg(in).outcome
      outcome.state match {
        case State.Success(next) => repeat(next)
        case State.Complete(next) => outcome.toYield(State.Success(Repeat(next)))
        case State.Error(next, errors) => outcome.toYield(State.Error(Repeat(next), errors))
        case panic: State.Panic => outcome.toYield(panic)
      }
    }
    repeat(stage)
  }
}
