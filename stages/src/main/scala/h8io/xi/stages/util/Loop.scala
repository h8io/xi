package h8io.xi.stages.util

import h8io.xi.stages.*

import scala.annotation.tailrec

final case class Loop[T, +E](stage: Stage.Endo[T, E]) extends Stage.SafeEndo[T, E] {
  def apply(in: T): Yield[T, T, E] = {
    @tailrec def loop(stage: Stage[T, T, E], in: T): Yield[T, T, E] = {
      val outcome = stage.safe(in).outcome()
      outcome.state match {
        case State.Success(updated) =>
          outcome match {
            case Outcome.Some(out, _, _) => loop(updated, out)
            case Outcome.None(_, _) => outcome.toYield(State.Success(Loop(updated)))
          }
        case State.Complete(updated) => outcome.toYield(State.Success(Loop(updated)))
        case failure => outcome.toYield(failure.map(Loop(_)))
      }
    }
    loop(stage, in)
  }
}
