package h8io.xi.stages

import scala.annotation.tailrec

final case class Loop[I, +E](stage: Stage[I, I, E]) extends Stage[I, I, E] {
  def apply(in: I): Yield[I, I, E] = {
    @tailrec def loop(stage: Stage[I, I, E], in: I): Yield[I, I, E] = {
      val outcome = stage(in).outcome
      outcome.state match {
        case State.Success(next) =>
          outcome match {
            case Outcome.Some(out, _, _) => loop(next, out)
            case Outcome.None(_, _) => outcome.toYield(State.Success(Loop(next)))
          }
        case State.Complete(next) => outcome.toYield(State.Success(Loop(next)))
        case State.Error(next, errors) => outcome.toYield(State.Error(Loop(next), errors))
        case panic: State.Panic => outcome.toYield(panic)
      }
    }
    loop(stage, in)
  }
}
