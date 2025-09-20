package h8io.xi.stages.std

import h8io.xi.stages.{Stage, State, Yield}

import scala.annotation.tailrec

final case class Repeat[-I, +O, +E](stage: Stage[I, O, E]) extends Stage[I, O, E] {
  self =>

  override def apply(in: I): Yield[I, O, E] = {
    @tailrec def repeat(stage: Stage[I, O, E]): Yield[I, O, E] = {
      val outcome = stage(in).outcome
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
