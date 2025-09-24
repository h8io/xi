package h8io.xi.stages.util

import h8io.xi.stages.{Outcome, Stage, State, Yield}

import scala.annotation.tailrec

final case class Loop[T, +E](stage: Stage[T, T, E]) extends Stage.Safe[T, T, E] {
  def apply(in: T): Yield[T, T, E] = {
    @tailrec def loop(stage: Stage[T, T, E], in: T): Yield[T, T, E] = {
      val outcome = stage.safe(in).outcome()
      outcome.state match {
        case State.Success(next) =>
          outcome match {
            case Outcome.Some(out, _, _) => loop(next, out)
            case Outcome.None(_, _) => outcome.toYield(State.Success(Loop(next)))
          }
        case State.Complete(next) => outcome.toYield(State.Success(Loop(next)))
        case failure => outcome.toYield(failure.map(Loop(_)))
      }
    }
    loop(stage, in)
  }
}
